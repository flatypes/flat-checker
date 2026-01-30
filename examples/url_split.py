# scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
type Scheme = lang(r'[A-Za-z][A-Za-z0-9+\-.]*')

# unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
type Unreserved = lang(r'[A-Za-z0-9\-\._~]')
# sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
#               / "*" / "+" / "," / ";" / "="
type SubDelims = lang(r'[!$&\'()*+,;=]')
# pct-encoded   = "%" HEXDIG HEXDIG
type PctEncoded = lang(r'%[0-9A-Fa-f][0-9A-Fa-f]')

# IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
type IPvFuture = lang(r'v[0-9A-Fa-f]+\.({Unreserved}|{SubDelims}|:)+')
# IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
type IPLiteral = lang(r'\[{IPv6address}|{IPvFuture})\]')

# userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
type Userinfo = lang(r'({Unreserved}|{PctEncoded}|{SubDelims}|:)*')
# reg-name      = *( unreserved / pct-encoded / sub-delims )
type RegName = lang(r'({Unreserved}|{PctEncoded}|{SubDelims})*')
# host          = IP-literal / IPv4address / reg-name
type Host = lang(r'{IPLiteral}|{IPv4address}|{RegName}')
# port          = *DIGIT
type Port = lang(r'[0-9]*')
# authority     = [ userinfo "@" ] host [ ":" port ]
type NetLoc = lang(r'({Userinfo}@)?{Host}(:{Port})?')

# pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
type PChar = lang(r'{Unreserved}|{PctEncoded}|{SubDelims}|:|@')
# path-abempty  = *( "/" *pchar )
type Path = lang(r'(/{PChar}*)*')

# query         = *( pchar / "/" / "?" )
type Query = lang(r'({PChar}|/|\?)*')

# fragment      = *( pchar / "/" / "?" )
type Fragment = lang(r'({PChar}|/|\?)*')

# URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
# hier-part     = "//" authority path-abempty / ...
type URL = lang(r'{Scheme}://{NetLoc}{Path}(\?{Query})?(#{Fragment})?')
