Herbert
=======

[![Build Status](https://travis-ci.org/tazjin/herbert.svg)](https://travis-ci.org/tazjin/herbert)

[![Docker Status](http://dockeri.co/image/tazjin/herbert)](https://registry.hub.docker.com/u/tazjin/herbert/)

Herbert is a simple signing server for cloud environments in which newly created servers need the ability to provision their own certificates signed by an internal CA. This is intended for use with Ansible and on an environment like Google Compute Engine.

**A note on naming**: I wanted to name this project something related to guarding, and Kerberos came to mind. However, the name Kerberos is already associated with some pretty terrible things in the IT world. A combination of thinking about the name Kerberos and the Haskellism of starting project names with 'H' resulted in the name Herbert.

## Model

Herbert provides an HTTP server that receives certificate signing requests. The signing requests are written to acid-state and every new CSR will send an email to a specified email address. The responsible person(s) will then have to approve the signing manually.

The requesting machine can poll Herbert for the status of the request and eventually receive the signed certificates.

## Administration

Herbert can recognize several certificates as administration certificates, the command line tool `herbert-cli` can be used in combination with an administrative certificate to approve/decline a CSR and to revoke existing certificates.

## Technical details

Herbert internally uses OpenSSL for the processing of signing requests. The library used for bindings is [HsOpenSSL][].

All data is stored in [acid-state][].


[HsOpenSSL]: http://hackage.haskell.org/package/HsOpenSSL
[acid-state]: http://hackage.haskell.org/package/acid-state
