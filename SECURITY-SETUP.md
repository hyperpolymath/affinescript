# Security Configuration for affinescript.dev

## Overview

This site is configured with maximum security settings on Cloudflare's free tier, including:

- **TLS 1.3** minimum (no TLS 1.2 or older)
- **HSTS** with preload (max-age=31536000, includeSubDomains)
- **Strict SSL/TLS** mode
- **HTTP/2 and HTTP/3 (QUIC)** enabled
- **Brotli compression**
- **Consent-Aware HTTP** (GDPR/privacy compliance)
- **RFC 9116 compliant** security.txt

## DNS Configuration

### A Records (GitHub Pages)

Both root (@) and www use A records for consistency:

```
@    A    185.199.108.153
@    A    185.199.109.153
@    A    185.199.110.153
@    A    185.199.111.153

www  A    185.199.108.153
www  A    185.199.109.153
www  A    185.199.110.153
www  A    185.199.111.153
```

**Why A records for www instead of CNAME?**
- Consistent behavior with root domain
- No CNAME chain resolution needed
- Direct control over IP addresses
- Better for SEO (some crawlers prefer consistency)

### AAAA Records (IPv6)

```
@    AAAA  2606:50c0:8000::153
@    AAAA  2606:50c0:8001::153
@    AAAA  2606:50c0:8002::153
@    AAAA  2606:50c0:8003::153

www  AAAA  2606:50c0:8000::153
www  AAAA  2606:50c0:8001::153
www  AAAA  2606:50c0:8002::153
www  AAAA  2606:50c0:8003::153
```

### CAA Records (Certificate Authority Authorization)

```
@  CAA  128 issue "letsencrypt.org"
@  CAA  128 issuewild "letsencrypt.org"
@  CAA  128 issue "digicert.com"
@  CAA  128 iodef "mailto:security@affinescript.dev"
```

**Flag 128 = Critical**: If a CA doesn't understand the CAA record, it MUST refuse to issue the certificate.

### Email Security

```
@       TXT  "v=spf1 include:_spf.github.com ~all"
_dmarc  TXT  "v=DMARC1; p=reject; rua=mailto:security@affinescript.dev"
```

## Cloudflare Security Settings

### TLS/SSL
- **Minimum TLS Version**: 1.3
- **SSL Mode**: Strict (Full with certificate verification)
- **Always Use HTTPS**: On
- **Automatic HTTPS Rewrites**: On
- **Opportunistic Encryption**: On
- **TLS 1.3 0-RTT**: On

### HSTS (HTTP Strict Transport Security)
```
Strict-Transport-Security: max-age=31536000; includeSubDomains; preload
```

- **Max Age**: 31536000 seconds (1 year)
- **Include Subdomains**: Yes
- **Preload**: Yes (eligible for browser preload lists)

To add to Chrome's preload list: https://hstspreload.org/

### Security Headers
- **X-Content-Type-Options**: nosniff
- **X-Frame-Options**: SAMEORIGIN
- **X-XSS-Protection**: 1; mode=block
- **Referrer-Policy**: strict-origin-when-cross-origin
- **Permissions-Policy**: Configured via meta tags or headers

### Performance
- **HTTP/2**: Enabled
- **HTTP/3 (QUIC)**: Enabled
- **Brotli Compression**: Enabled
- **0-RTT Connection Resumption**: Enabled

### Bot Management
- **Security Level**: Medium (allows search bots)
- **Browser Integrity Check**: On
- **Challenge Passage**: 30 minutes
- **Email Obfuscation**: On

## Consent-Aware HTTP

This site implements consent-aware-http for GDPR/privacy compliance.

### Consent Categories

1. **Essential** (always on)
   - Core site functionality
   - Security features
   - Session management

2. **Functional**
   - Enhanced features
   - User preferences
   - Language settings

3. **Analytics**
   - Anonymous usage statistics
   - Performance monitoring
   - Error tracking

4. **Marketing**
   - Advertising
   - Campaign tracking
   - Social media integration

5. **Personalization**
   - Customized content
   - Recommendations
   - User profiling

### Implementation

**Cloudflare Worker** (consent-aware-http.js) intercepts all requests and:
1. Checks for `user-consent` cookie
2. Validates consent level matches resource requirements
3. Returns 403 if consent not granted
4. Passes request to origin if consent valid

**Frontend** includes consent banner allowing users to:
- View current consent settings
- Grant/revoke consent per category
- Export consent preferences
- Delete all tracking data

### Testing Consent

```bash
# Without consent cookie (essential only)
curl https://affinescript.dev/api/analytics
# → 403 Forbidden (requires analytics consent)

# With analytics consent
curl -b "user-consent=%7B%22analytics%22%3Atrue%7D" \
  https://affinescript.dev/api/analytics
# → 200 OK

# Essential resources always work
curl https://affinescript.dev/
# → 200 OK (no consent needed)
```

## .well-known/security.txt

RFC 9116 compliant security contact information:

```
https://affinescript.dev/.well-known/security.txt
```

Contains:
- Security contact emails
- GitHub security advisory links
- PGP encryption keys
- Security policy links
- Acknowledgments page
- Consent-aware-http endpoints
- Expiration date (1 year)

## Verification

### Check DNS Records
```bash
dig affinescript.dev A +short
dig affinescript.dev AAAA +short
dig affinescript.dev CAA +short
dig _dmarc.affinescript.dev TXT +short
```

### Check TLS Configuration
```bash
curl -I https://affinescript.dev | grep -i strict-transport
```

### Check security.txt
```bash
curl https://affinescript.dev/.well-known/security.txt
```

### SSL Labs Test
https://www.ssllabs.com/ssltest/analyze.html?d=affinescript.dev

**Expected Grade**: A+ (with HSTS preload)

### SecurityHeaders.com
https://securityheaders.com/?q=affinescript.dev

**Expected Grade**: A+ (with all headers configured)

## Reporting Security Issues

**DO NOT** open public GitHub issues for security vulnerabilities.

Instead:
1. Email: security@affinescript.dev
2. GitHub Security Advisory: https://github.com/hyperpolymath/affinescript/security/advisories/new
3. PGP encrypted: https://keys.openpgp.org/search?q=jonathan.jewell@open.ac.uk

We aim to respond within 48 hours.

## Privacy & Consent Issues

For privacy concerns or consent management:
- Email: privacy@affinescript.dev
- Consent settings: https://affinescript.dev/privacy#consent
- Data export/deletion: https://affinescript.dev/privacy#your-rights

## License

Security configuration: PMPL-1.0-or-later
Documentation: CC-BY-SA-4.0
