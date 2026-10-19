# Libera SASL EXTERNAL with a YubiKey

ERC uses its built-in SASL EXTERNAL support over native Emacs/GnuTLS.
The private key remains on the token; host-specific token selection and the
public client certificate live under the Git-ignored `private/erc/` directory.

## Local setup

This connector requires Emacs with `erc-sasl`, native GnuTLS with PKCS#11
support, and a YKCS11 provider. On macOS, install the provider with
`brew install yubico-piv-tool`, then configure
`~/.config/pkcs11/modules/cq-yubikey.module`:

```text
module: /opt/homebrew/opt/yubico-piv-tool/lib/libykcs11.dylib
```

Use an existing token-backed key and matching public certificate enrolled
with Libera NickServ. Adapt the provider path and PKCS#11 object ID to the
local installation and selected slot. Do not regenerate an enrolled key as
part of installing this Emacs configuration.

Create `~/.config/emacs/private/erc/config.el` with a host-specific URI:

```elisp
(setq cq-erc-libera-key-uri
      "pkcs11:serial=REPLACE_WITH_TOKEN_SERIAL;id=%01;type=private")
```

Replace the placeholder with the intended token's serial; object ID `01`
selects PIV slot 9a with YKCS11. Keep token identifiers in this private file.
Do not put a PIN in the URI or configuration. This file is loaded after the
shared defaults when ERC loads; startup also works when it is absent.
`cq-erc-libera` rejects missing or non-PKCS#11 key settings before connecting
or requesting a PIN.

Put the matching public certificate at `private/erc/libera-yubikey.pem`.
For another location, set `cq-erc-libera-certificate` in the same private
configuration file. Both the private configuration and certificate are
ignored by Git. Keep provisioning, enrollment and cleanup records in the
separate private inventory, without storing private-key material or PINs.

## Daily use

`C-c i` / `M-x cq-erc-libera` prompts for the PIV PIN in Emacs. A token
configured to require touch also requires physical confirmation. `C-g`
cancels during ordinary Lisp/network waiting, but cannot interrupt a blocking
native token operation. The 45-second connection deadline is checked by Lisp
and is not a hard timeout for native calls.

Automatic reconnect is disabled. Reconnect manually after an interruption
so PIN and touch requests remain deliberate.

`M-x cq-erc-libera-password` explicitly uses the previous TLS/NickServ login
from auth-source for recovery. The YubiKey command never falls back to it.
Normal channel autojoin occurs only after the SASL guard accepts registration.

## Security and failure behavior

The connector restricts the destination to `irc.libera.chat:6697` and enables
both certificate-chain and hostname verification with `:verify-error t`.
It requires a readable client certificate before requesting a PIN. Empty
PIN input cancels. Failed or cancelled connection attempts close their
process. ERC disconnects on SASL failure, and the local registration guard
rejects a welcome received without SASL success numeric 903.

The Lisp PIN string is cleared and removed from the TLS parameter list after
the handshake, including error and cancellation paths. GnuTLS retains a PIN
copy in native connection credentials. Token-backed keys prevent private-key
export; this does not protect against a compromised running Emacs process.

Unplugging the token does not terminate an established TLS connection. The
key is needed again for a fresh handshake. Enrolled credentials and recovery
methods must be managed separately from this Emacs configuration.

## Validation

Run the offline checks and the full startup smoke test:

```sh
emacs -Q --batch -l tests/erc-yubikey-test.el
emacs --batch --init-directory=. --load init.el --eval '(message "startup ok")'
```

The offline tests load the actual IRC block from `config.org` with temporary
private-settings fixtures. They never load the host's private IRC settings,
contact IRC, or access a token. They cover private-settings loading,
missing-setting errors, endpoint/certificate checks, verification parameters,
PIN cleanup, failed/cancelled connection cleanup, and the SASL registration
guard. They do not establish hardware behavior.

For a live validation on a configured host, check that the server observes
the intended client certificate (WHOIS numeric 276) and that ERC receives
SASL success numeric 903. Keep host-specific results and failure-case coverage
in the private inventory.

## Sources

- [Libera CertFP setup and enrollment](https://libera.chat/guides/certfp)
- [ERC SASL documentation](https://www.gnu.org/software/emacs/manual/html_node/erc/SASL.html)
- [Yubico YKCS11](https://developers.yubico.com/yubico-piv-tool/YKCS11/)
- [GnuTLS PIN callback and credential storage](https://github.com/gnutls/gnutls/blob/3.8.13/lib/cert-cred-x509.c)
