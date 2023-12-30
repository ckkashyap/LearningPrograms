#include <uv.h>
#include <SDL2/SDL.h>
#include <stdio.h>

#define WINDOW_WIDTH 800
#define WINDOW_HEIGHT 600
#define CIRCLE_RADIUS 10

// UV loop and SDL loop handlers
uv_loop_t* uv_loop;
SDL_Window* window;
SDL_Renderer* renderer;

// UV TCP handle
uv_tcp_t tcp;

// Message structure for communication between threads
typedef struct {
	uv_connect_t connect_req;
	struct sockaddr_in dest;
} ConnectMessage;

// Queue for messages from SDL to libuv
uv_async_t message_queue;
uv_mutex_t message_mutex;
ConnectMessage pending_message;

// HTTP GET request data
const char* http_request = "GET / HTTP/1.1\r\n"
"Host: localhost:8080\r\n"
"Connection: close\r\n"
"\r\n";

// Buffer for reading HTTP response
uv_buf_t response_buffer;


// Event type for SDL
#define SDL_EVENT_TYPE (SDL_USEREVENT + 1)

// SDL event data structure
typedef struct {
	uv_tcp_t* tcp;
} SDLMessageEventData;


// Callback for allocating read buffer
void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
	buf->base = (char*) malloc(suggested_size);
	buf->len = suggested_size;
}

// Callback for handling TCP read completion
void on_tcp_read(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
	if (nread > 0) {
		// Handle the received data here
		// For simplicity, we'll just print the response
		printf("Received data: %.*s\n", (int)nread, buf->base);

		// You can parse the response and update your rendering logic here

		// Post SDL event to draw a circle
		SDL_Event event;
		event.type = SDL_EVENT_TYPE;
		SDLMessageEventData* eventData = (SDLMessageEventData*)malloc(sizeof(SDLMessageEventData));
		eventData->tcp = (uv_tcp_t*)stream;
		event.user.data1 = eventData;
		SDL_PushEvent(&event);

		// Free the buffer
		free(buf->base);
	} else if (nread < 0) {
		fprintf(stderr, "TCP read failed: %s\n", uv_strerror(nread));
	}

	// Stop reading data from the server
	uv_read_stop(stream);

	// Cleanup the TCP handle
	uv_close((uv_handle_t*)stream, NULL);
}

// Callback for handling TCP write completion
void on_tcp_write(uv_write_t* req, int status) {
	if (status == 0) {
		printf("HTTP GET request sent successfully\n");

		// Start reading data from the server
		uv_read_start((uv_stream_t*)&tcp, alloc_buffer, on_tcp_read);
	} else {
		fprintf(stderr, "TCP write failed: %s\n", uv_strerror(status));

		// Cleanup the write request handle
		uv_close((uv_handle_t*)req, NULL);
	}
}


// Callback for handling TCP connection
void on_tcp_connect(uv_connect_t* req, int status) {
	if (status == 0) {
		printf("TCP Connection successful\n");

		// Connection successful, send the HTTP request
		uv_buf_t buf = uv_buf_init((char*)http_request, strlen(http_request));
		uv_write_t* write_req = (uv_write_t*)malloc(sizeof(uv_write_t));
		uv_write(write_req, (uv_stream_t*)&tcp, &buf, 1, on_tcp_write);
	} else {
		fprintf(stderr, "TCP connection failed: %s\n", uv_strerror(status));

		// Cleanup the connect request handle
		uv_close((uv_handle_t*)req->handle, NULL);
	}
}



// SDL event handler
void handle_sdl_events() {
	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		switch (event.type) {
			case SDL_QUIT:
				SDL_Quit();
				exit(0);
				break;

			case SDL_EVENT_TYPE:
				// Draw a circle when SDL_EVENT_TYPE is received
				{
					SDLMessageEventData* eventData = (SDLMessageEventData*)event.user.data1;
					printf("AAAA\n");

					int mouseX, mouseY;
					SDL_GetMouseState(&mouseX, &mouseY);
					int r = 255, g = 0, b = 0;

					// Draw a small circle on the window at the mouse coordinates with the obtained color
					SDL_SetRenderDrawColor(renderer, r, g, b, 255);
					SDL_RenderDrawPoint(renderer, mouseX, mouseY);
					SDL_RenderPresent(renderer);

					free(eventData);
				}
				break;


			case SDL_MOUSEBUTTONDOWN:
				if (event.button.button == SDL_BUTTON_LEFT) {
					printf("click\n");
					// Make an HTTP GET request to http://localhost:8080
					uv_mutex_lock(&message_mutex);
					pending_message.dest.sin_family = AF_INET;
					pending_message.dest.sin_port = htons(8080);
					uv_ip4_addr("127.0.0.1", 8080, &pending_message.dest);
					uv_mutex_unlock(&message_mutex);

					uv_async_send(&message_queue);
				}
				break;
			default:
				break;
		}
	}
}

// Callback for handling UV loop events
void uv_async_callback(uv_async_t* handle) {
	printf("callback\n");
	// Process pending messages
	uv_mutex_lock(&message_mutex);
	ConnectMessage message = pending_message;
	uv_mutex_unlock(&message_mutex);

	if ( !uv_is_closing((const uv_handle_t*)&tcp)) {
		// Handle the message
		uv_connect_t* connect_req = &message.connect_req;
		uv_tcp_connect(connect_req, &tcp, (const struct sockaddr*)&message.dest, on_tcp_connect);
	}
	else
	{
		uv_tcp_init(uv_loop, &tcp);
		uv_connect_t* connect_req = &message.connect_req;
		uv_tcp_connect(connect_req, &tcp, (const struct sockaddr*)&message.dest, on_tcp_connect);
	}
}

int main() {
	// Initialize SDL
	SDL_Init(SDL_INIT_VIDEO);

	// Create SDL window and renderer
	window = SDL_CreateWindow("SDL+libuv Example", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, 0);
	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);

	// UV loop initialization
	uv_loop = uv_default_loop();

	// TCP handle initialization
	uv_tcp_init(uv_loop, &tcp);

	// Initialize the message queue and mutex
	uv_async_init(uv_loop, &message_queue, uv_async_callback);
	uv_mutex_init(&message_mutex);


	// Main SDL loop
	while (1) {
		handle_sdl_events();

		// Run the UV loop
		uv_run(uv_loop, UV_RUN_NOWAIT);

		SDL_Delay(10);

	}

	// Cleanup libuv resources
	uv_close((uv_handle_t*)&tcp, NULL);

	return 0;
}

