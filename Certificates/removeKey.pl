#!/usr/bin/perl

use strict;
use warnings;

my$key=$ARGV[0];

`openssl rsa -in $key -out $key.nopassword.pem`;
