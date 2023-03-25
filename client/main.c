/*
 *  Copyright 2022 The OpenSSL Project Authors. All Rights Reserved.
 *
 *  Licensed under the Apache License 2.0 (the "License").  You may not use
 *  this file except in compliance with the License.  You can obtain a copy
 *  in the file LICENSE in the source distribution or at
 *  https://www.openssl.org/source/license.html
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

#include "sensei.h"

typedef unsigned char   bool;
#define true            1
#define false           0

SSL_CTX* create_context()
{
    const SSL_METHOD *method;
    SSL_CTX *ctx;

    method = TLS_client_method();

    ctx = SSL_CTX_new(method);
    if (ctx == NULL) {
        perror("Unable to create SSL context");
        ERR_print_errors_fp(stderr);
        exit(EXIT_FAILURE);
    }

    return ctx;
}


void configure_client_context(SSL_CTX *ctx)
{
    /*
     * Configure the client to abort the handshake if certificate verification
     * fails
     */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);

    /*
     * use the default system certificate trust store
     * Can be overriden by env variables CA_PATH?
     */
    if (!SSL_CTX_set_default_verify_paths(ctx)) {
        ERR_print_errors_fp(stderr);
        exit(EXIT_FAILURE);
    }

}

void usage()
{
    printf("Usage: sslecho s\n");
    printf("       --or--\n");
    printf("       sslecho c ip\n");
    printf("       c=client, s=server, ip=dotted ip of server\n");
    exit(1);
}

// Single JSON POST query
/* static const char* POST_QUERY = */
/*   "POST %s HTTP/1.1\r\nHost: %s\r\nContent-Length: %d\r\nContent-type: application/json\r\n\r\n%s"; */

int main(int argc, char **argv)
{
    int result;

    SSL_CTX *ssl_ctx = NULL;
    SSL *ssl = NULL;

    int server_skt = -1;
    int client_skt = -1;

    struct addrinfo hints;
    struct addrinfo *addr_info, *addr;
    int resolv_err; // error code

    /* used by getline relying on realloc, can't be statically allocated */
    char *txbuf = NULL;
    size_t txcap = 0;

    int txlen;

    char rxbuf[128];
    size_t rxcap = sizeof(rxbuf);
    int rxlen;

    char *server_name = NULL;
    bool end_of_input = false;

    /* options parsing */
    struct client_options opts;

    /* Splash */
    printf("\nsslecho : Simple TLS Client (OpenSSL 3.0.1-dev) : %s : %s\n\n", __DATE__,
    __TIME__);

    if (parse_options(&opts, argc, argv) < 0) {
      usage();
    }

    /* Create context used by both client and server */
    ssl_ctx = create_context();

    {

        printf("We are the client\n\n");

        /* Configure client context so we verify the server correctly */
        configure_client_context(ssl_ctx);

        /* Obtain address for host */

        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 */
        hints.ai_socktype = SOCK_STREAM; /* Stream socket */
        hints.ai_flags = 0;
        hints.ai_protocol = 0;          /* Any protocol */

        resolv_err = getaddrinfo(opts.server_name, "443", &hints, &addr_info);

        if (resolv_err != 0) {
          fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(resolv_err));
          exit(EXIT_FAILURE);
        }

        /* try connecting to each addr_info returned by getaddrinfo */
        for (addr = addr_info; addr != NULL; addr = addr->ai_next) {
          client_skt = socket(addr->ai_family, addr->ai_socktype,
                              addr->ai_protocol);
          if (client_skt == -1)
            continue;

          if (connect(client_skt, addr->ai_addr, addr->ai_addrlen) != -1) {
            printf("TCP connection to server successful\n");
            break;                  /* Success */
          }

          close(client_skt);
          client_skt = -1;
        }

        if(client_skt == -1) {
            perror("Unable to TCP connect to server");
            goto exit;
        }

        /* Create client SSL structure using dedicated client socket */
        ssl = SSL_new(ssl_ctx);
        SSL_set_fd(ssl, client_skt);
        /* Set hostname for SNI */
        SSL_set_tlsext_host_name(ssl, opts.server_name);
        /* Configure server hostname check */
        SSL_set1_host(ssl, opts.server_name);

        /* Now do SSL connect with server */
        if (SSL_connect(ssl) == 1) {

            printf("SSL connection to server successful\n\n");

            /* Loop to send input from keyboard */
            while (!end_of_input) {
                /* Get a line of input */
                txlen = getline(&txbuf, &txcap, stdin);
                /* Exit loop on error */
                if (txlen < 0 || txbuf == NULL) {
                    break;
                }

                /* complete line with CRLF */
                txlen = complete_with_crlf(&txbuf, &txcap, txlen);

                /* Send it to the server after checking proper CRLF termination */
                if ((result = SSL_write(ssl, txbuf, txlen)) <= 0) {
                    printf("Server closed connection\n");
                    ERR_print_errors_fp(stderr);
                    break;
                }

                if(txlen == 2 && txbuf[0] == '\r' && txbuf[1] == '\n') {
                  end_of_input = true;
                }
            }

            /* Wait for the echo */
            rxlen = SSL_read(ssl, rxbuf, rxcap);
            if (rxlen <= 0) {
              printf("Server closed connection\n");
              ERR_print_errors_fp(stderr);
            } else {
              /* Show it */
              rxbuf[rxlen] = 0;
              printf("Received: %s", rxbuf);
            }

            printf("Client exiting...\n");
        } else {

            printf("SSL connection to server failed\n\n");

            ERR_print_errors_fp(stderr);
        }
    }
    exit:
    /* Close up */
    if (ssl != NULL) {
        SSL_shutdown(ssl);
        SSL_free(ssl);
    }
    SSL_CTX_free(ssl_ctx);

    if (client_skt != -1)
        close(client_skt);
    if (server_skt != -1)
        close(server_skt);

    if (txbuf != NULL && txcap > 0)
        free(txbuf);

    printf("sslecho exiting\n");

    return 0;
}
