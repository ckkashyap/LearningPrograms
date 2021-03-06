#!/usr/bin/perl

use strict;
use warnings;

my $SCRIPTDIR=`pwd`;
chomp$SCRIPTDIR;
my$KEY_PASSWORD="helloworld123";

# Create intermediate certificates
my$INTERMEDIATE_DIR="$SCRIPTDIR/Intermediates";
my$CADIR="$SCRIPTDIR/CA";
`rm -rf $INTERMEDIATE_DIR`;
mkdir $INTERMEDIATE_DIR;
chdir $INTERMEDIATE_DIR;
mkdir $_ for qw(certs crl csr newcerts private);
`chmod 700 private`;
`touch index.txt`;
` echo 1000 > serial`;
`echo 1000 > crlnumber`;
# Create key
`openssl genrsa -aes256 -out private/ca.key.pem -passout pass:$KEY_PASSWORD 4096`;
`chmod 400 private/ca.key.pem`;
# Create 
# Create a cert request and self-signed CA certificate
open CONF,">openssl.cnf";
print CONF getOpenSSLConf($INTERMEDIATE_DIR, "policy_loose");
close CONF;
`openssl req -config openssl.cnf -key private/ca.key.pem -passin pass:$KEY_PASSWORD -new -sha256 -out csr/ca.csr.pem`;

# Create the certificate
`openssl ca -batch -config $CADIR/openssl.cnf -extensions v3_intermediate_ca -passin pass:$KEY_PASSWORD -days 3650 -notext -md sha256 -in $INTERMEDIATE_DIR/csr/ca.csr.pem  -out $INTERMEDIATE_DIR/certs/ca.cert.pem`;
`chmod 444 certs/ca.cert.pem`;
# Print the certificate
system("openssl x509 -noout -text -in certs/ca.cert.pem");

system("openssl verify -CAfile $CADIR/certs/ca.cert.pem certs/ca.cert.pem");

sub getOpenSSLConf
{
my($dir, $policy)=@_;
return <<"END"
[ ca ]
# `man ca`
default_ca = CA_default

[ CA_default ]
# Directory and file locations.
certs             = $dir/certs
crl_dir           = $dir/crl
new_certs_dir     = $dir/newcerts
database          = $dir/index.txt
serial            = $dir/serial
RANDFILE          = $dir/private/.rand

# The root key and root certificate.
private_key       = $dir/private/ca.key.pem
certificate       = $dir/certs/ca.cert.pem

# For certificate revocation lists.
crlnumber         = $dir/crlnumber
crl               = $dir/crl/ca.crl.pem
crl_extensions    = crl_ext
default_crl_days  = 30

# SHA-1 is deprecated, so use SHA-2 instead.
default_md        = sha256

name_opt          = ca_default
cert_opt          = ca_default
default_days      = 375
preserve          = no
policy            = $policy

[ policy_strict ]
# The root CA should only sign intermediate certificates that match.
# See the POLICY FORMAT section of `man ca`.
countryName             = match
stateOrProvinceName     = match
organizationName        = match
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ policy_loose ]
# Allow the intermediate CA to sign a more diverse range of certificates.
# See the POLICY FORMAT section of the `ca` man page.
countryName             = optional
stateOrProvinceName     = optional
localityName            = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ req ]
# Options for the `req` tool (`man req`).
prompt = no
default_bits        = 2048
distinguished_name  = req_distinguished_name
string_mask         = utf8only

# SHA-1 is deprecated, so use SHA-2 instead.
default_md          = sha256

# Extension to add when the -x509 option is used.
x509_extensions     = v3_ca

[ req_distinguished_name ]
# See <https://en.wikipedia.org/wiki/Certificate_signing_request>.
countryName                     = US
stateOrProvinceName             = Washington
localityName                    = Redmond
0.organizationName              = Imaginary
organizationalUnitName          = Imaginary unit
commonName                      = ImaginaryInter

[ v3_ca ]
# Extensions for a typical CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ v3_intermediate_ca ]
# Extensions for a typical intermediate CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true, pathlen:0
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ usr_cert ]
# Extensions for client certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = client, email
nsComment = "OpenSSL Generated Client Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, nonRepudiation, digitalSignature, keyEncipherment
extendedKeyUsage = clientAuth, emailProtection

[ server_cert ]
# Extensions for server certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = server
nsComment = "OpenSSL Generated Server Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer:always
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth

[ crl_ext ]
# Extension for CRLs (`man x509v3_config`).
authorityKeyIdentifier=keyid:always

[ ocsp ]
# Extension for OCSP signing certificates (`man ocsp`).
basicConstraints = CA:FALSE
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, digitalSignature
extendedKeyUsage = critical, OCSPSigning
END
}
