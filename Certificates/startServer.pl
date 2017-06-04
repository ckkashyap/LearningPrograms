#!/usr/bin/perl

use strict;
use warnings;

`openssl s_server -accept 1443 -cert Server/cert.pem -key Server/server.key.pem  -WWW`;
