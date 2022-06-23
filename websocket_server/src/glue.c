#include <libwebsockets.h>
#include <libpq-fe.h>

struct buffer {
	size_t len;
	uint8_t *data;
};

static Value *buffer_to_list_bits8(struct buffer buf) {
	Value_Constructor *head = newConstructor(0, 0, "");
	for (size_t i = 0; i < buf.len; ++i) {
		Value_Constructor *cons = newConstructor(2, 1, "");
		cons->args[0] = (Value *)makeBits8(buf.data[buf.len - i - 1]);
		cons->args[1] = (Value *)head;
		head = cons;
	}
	return (Value *)head;
}

static void buffer_from_list_bits8(struct buffer buf, Value *val) {
	Value_Constructor *head = (Value_Constructor *)val;
	size_t i = 0;
	while (head->tag == 1 && i < buf.len) {
		buf.data[i++] = ((Value_Bits8 *)head->args[0])->ui8;
		head = (Value_Constructor *)head->args[1];
	}
}

static size_t len_of_list(Value *val) {
	Value_Constructor *head = (Value_Constructor *)val;
	size_t len = 0;
	while (head->tag == 1) {
		++len;
		head = (Value_Constructor *)head->args[1];
	}
	return len;
}

struct array {
	size_t len;
	Value **values;
};

static Value *array_to_list(struct array array) {
	Value_Constructor *head = newConstructor(0, 0, "");
	for (size_t i = 0; i < array.len; ++i) {
		Value_Constructor *cons = newConstructor(2, 1, "");
		cons->args[0] = array.values[array.len - i - 1];
		cons->args[1] = (Value *)head;
		head = cons;
	}
	return (Value *)head;
}

static struct array array_from_list(Value *val) {
	const size_t len = len_of_list(val);
	struct array array = {
		.len = len,
		.values = malloc(len * sizeof(Value *)),
	};

	Value_Constructor *head = (Value_Constructor *)val;
	size_t i = 0;
	while (head->tag == 1 && i < array.len) {
		array.values[i++] = head->args[0];
		head = (Value_Constructor *)head->args[1];
	}

	return array;
}

static Value *makeNothing() {
	return (Value *)newConstructor(0, 0, "");
}

static Value *makeJust(Value *val) {
	Value_Constructor *just = newConstructor(1, 1, "");
	just->args[0] = val;
	return (Value *)just;
}

// extra arg: conn pointer
// String -> List String -> IO (Maybe $ List String)
static Value *exec_impl(Value_Arglist *arglist) {
	PGconn *conn = ((Value_Pointer *)arglist->args[0])->p;
	const char *command = ((Value_String *)arglist->args[1])->str;
	struct array params_array = array_from_list(arglist->args[2]);
	Value_World *world = (Value_World *)arglist->args[3];

	const char **params = malloc(params_array.len * sizeof(char *));
	for (size_t i = 0; i < params_array.len; ++i) {
		params[i] = ((Value_String *)params_array.values[i])->str;
	}

	Value *result;

	PGresult *res = PQexecParams(conn, command, params_array.len, NULL, params, NULL, NULL, 0);
	switch (PQresultStatus(res)) {
		case PGRES_COMMAND_OK:
		case PGRES_NONFATAL_ERROR:
			result = makeJust((Value *)newConstructor(0, 0, ""));
			goto finish;
		case PGRES_TUPLES_OK: {
			int ncols = PQnfields(res);
			if (!(ncols == 1 || PQfformat(res, 0) == 0)) goto bad;

			int nrows = PQntuples(res);
			struct array rows = {
				.len = nrows,
				.values = malloc(nrows * sizeof(Value *)),
			};
			for (int i = 0; i < nrows; ++i) {
				const char *value = PQgetvalue(res, i, 0);
				rows.values[i] = (Value *)makeString((char *)value);
			}
			result = makeJust(array_to_list(rows));
			free(rows.values);
			goto finish;
		}
		default:
			goto bad;
	}
bad:
	result = makeNothing();
finish:
	PQclear(res);
	free(params);
	free(params_array.values);

	return result;
}

void dummy_notice_processor(void *arg, const char *message) {

}

// (connection_string : String) -> IO (Maybe DbConnection)
static Value *db_connect_impl(Value_Arglist *arglist) {
	const char *connection_string = ((Value_String *)arglist->args[0])->str;
	Value_World *world = (Value_World *)arglist->args[1];

	PGconn *conn = PQconnectdb(connection_string);
	if (PQstatus(conn)) goto fail;

	PQsetNoticeProcessor(conn, dummy_notice_processor, NULL);

	Value_Constructor *ctor = newConstructor(2, 0, "");
	Value_Arglist *exec_arglist = newArglist(3, 4);
	exec_arglist->args[0] = (Value *)makePointer(conn);
	ctor->args[0] = (Value *)makeClosureFromArglist(exec_impl, exec_arglist);
	ctor->args[1] = NULL;
	return makeJust((Value *)ctor);
fail:
	return makeNothing();
}

// libwebsockets stuff based on:
// https://github.com/iamscottmoyers/simple-libwebsockets-example/blob/master/server.c
const size_t RX_BUFFER_BYTES = 8;

// Shared context data
struct ctx {
	Value *established;
	Value *world;
};

// Per session storage
struct pss {
	unsigned char *msg_buffer;
	size_t msg_buffer_len;
	size_t msg_len;

	Value *receive;
	Value *writable;
	Value *closed;
};

static void append_message_fragment(
	struct pss *pss,
	void *in,
	size_t len,
	size_t remaining
) {
	// Make sure that `len` more fits into `pss->msg_buffer`
	if (!pss->msg_buffer || pss->msg_len + len > pss->msg_buffer_len) {
		pss->msg_buffer_len = pss->msg_len + len + remaining;
		pss->msg_buffer = realloc(pss->msg_buffer, pss->msg_buffer_len);
	}

	// Append to buffer
	memcpy(pss->msg_buffer + pss->msg_len, in, len);
	pss->msg_len += len;
}

static void done_message(struct ctx *ctx, struct pss *pss, struct lws *wsi) {
	Value *list_bits8 = buffer_to_list_bits8((struct buffer){
		.len = pss->msg_len,
		.data = pss->msg_buffer
	});
	Value *primIO_val = apply_closure(pss->receive, list_bits8);
	Value *unit_val = apply_closure(primIO_val, ctx->world);
	removeReference(list_bits8);
	removeReference(primIO_val);
	removeReference(unit_val);

	pss->msg_len = 0;
}

static Value *callback_on_writable_impl(Value_Arglist *arglist) {
	struct lws *wsi = ((Value_Pointer *)arglist->args[0])->p;
	Value_World *world = (Value_World *)arglist->args[1];

	lws_callback_on_writable(wsi);

	return NULL;
}

static int cb_main(
	struct lws *wsi,
	enum lws_callback_reasons reason,
	void *user,
	void *in,
	size_t len
) {
	struct ctx *ctx = lws_context_user(lws_get_context(wsi));
	struct pss *pss = user;

	switch (reason) {
		case LWS_CALLBACK_ESTABLISHED: {
			Value_Constructor *ctor = newConstructor(2, 0, "");
			Value_Arglist *arglist = newArglist(1, 2);
			arglist->args[0] = (Value *)makePointer(wsi);
			ctor->args[0] = (Value *)makeClosureFromArglist(callback_on_writable_impl, arglist);
			ctor->args[1] = NULL;

			Value *primIO_val = apply_closure(ctx->established, (Value *)ctor);
			Value_Constructor *handlers =
				(Value_Constructor *)apply_closure(primIO_val, ctx->world);
			removeReference(primIO_val);
			removeReference((Value *)ctor);

			*pss = (struct pss){
				.receive = newReference(handlers->args[0]),
				.writable = newReference(handlers->args[1]),
				.closed = newReference(handlers->args[2]),
			};

			removeReference((Value *)handlers);

			break;
		}
		case LWS_CALLBACK_CLOSED: {
			removeReference(apply_closure(pss->closed, ctx->world));

			removeReference(pss->receive);
			removeReference(pss->writable);
			removeReference(pss->closed);
			free(pss->msg_buffer);
			break;
		}
		case LWS_CALLBACK_RECEIVE: {
			const size_t remaining = lws_remaining_packet_payload(wsi);

			append_message_fragment(pss, in, len, remaining);

			if (remaining == 0 && lws_is_final_fragment(wsi)) {
				done_message(ctx, pss, wsi);
			}
			break;
		}
		case LWS_CALLBACK_SERVER_WRITEABLE: {
			Value_Constructor *maybe_list_bits8 =
				(Value_Constructor *)apply_closure(pss->writable, ctx->world);
			if (maybe_list_bits8->tag == 1) {
				Value *list_bits8 = maybe_list_bits8->args[0];
				size_t len = len_of_list(list_bits8);

				uint8_t *data = malloc(LWS_PRE + len);
				struct buffer buf = {
					.len = len,
					.data = data + LWS_PRE,
				};
				buffer_from_list_bits8(buf, list_bits8);

				lws_write(wsi, buf.data, buf.len, LWS_WRITE_BINARY);

				free(data);
			}
			removeReference((Value *)maybe_list_bits8);
			break;
		}
		default:
			break;
	}

	return 0;
}

static Value *listen_impl(Value_Arglist *arglist) {
	Value_Closure *established = (Value_Closure *)arglist->args[0];
	Value_World *world = (Value_World *)arglist->args[1];

	struct ctx ctx_data = {
		.established = (Value *)established,
		.world = (Value *)world,
	};
	struct lws_protocols protocols[] = {
		{
			.name = "http",
			.callback = lws_callback_http_dummy,
		},
		{
			.name = "main",
			.callback = cb_main,
			.per_session_data_size = sizeof(struct pss),
			.rx_buffer_size = RX_BUFFER_BYTES,
		},
		LWS_PROTOCOL_LIST_TERM,
	};
	struct lws_context_creation_info info = {
		.gid = -1,
		.uid = -1,
		.user = &ctx_data,
		.port = 8000,
		.protocols = protocols,
	};
	struct lws_context *context = lws_create_context(&info);
	while (1) lws_service(context, 0);
	lws_context_destroy(context);

	return NULL;
}

static void *get_glue() {
	Value_Constructor *ctor = newConstructor(2, 0, "");
	ctor->args[0] = (Value *)makeClosureFromArglist(listen_impl, newArglist(2, 2));
	ctor->args[1] = (Value *)makeClosureFromArglist(db_connect_impl, newArglist(2, 2));
	return ctor;
}
