#include <uv.h>

uv_loop_t *loop;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char*) malloc(suggested_size);
    buf->len = suggested_size;
}


void on_read(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        // Simple response for a GET request
        const char *response = "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nHello, World!";
        uv_write_t *write_req = (uv_write_t*)malloc(sizeof(uv_write_t));

        uv_buf_t write_buf = uv_buf_init((char*)response, strlen(response));
        uv_write(write_req, client, &write_buf, 1, NULL);

        uv_close((uv_handle_t*)client, NULL);
    } else if (nread < 0) {
        if (nread != UV_EOF) {
            fprintf(stderr, "Read error: %s\n", uv_strerror(nread));
        }
        uv_close((uv_handle_t*)client, NULL);
    }

    // Don't forget to free the buffer
    if (buf->base) {
        free(buf->base);
    }
}

void on_connection(uv_stream_t *server, int status) {
    if (status < 0) {
        fprintf(stderr, "Error on connection: %s\n", uv_strerror(status));
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*)malloc(sizeof(uv_tcp_t));
    uv_tcp_init(loop, client);

    if (uv_accept(server, (uv_stream_t*)client) == 0) {
        uv_read_start((uv_stream_t*)client, alloc_buffer, on_read);
    } else {
        uv_close((uv_handle_t*)client, NULL);
    }
}

int main() {
    loop = uv_default_loop();

    uv_tcp_t server;
    uv_tcp_init(loop, &server);

    struct sockaddr_in addr;
    uv_ip4_addr("0.0.0.0", 8080, &addr);

    uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0);
    int r = uv_listen((uv_stream_t*)&server, 128, on_connection);

    if (r) {
        fprintf(stderr, "Listen error: %s\n", uv_strerror(r));
        return 1;
    }

    return uv_run(loop, UV_RUN_DEFAULT);
}

