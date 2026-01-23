class AddressValueError(ValueError):
  """A Value Error related to the address."""
  pass


# dec-octet   = DIGIT                 ; 0-9
#             / %x31-39 DIGIT         ; 10-99
#             / "1" 2DIGIT            ; 100-199
#             / "2" %x30-34 DIGIT     ; 200-249
#             / "25" %x30-35          ; 250-255
type DecOctet = lang(r'[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5]')


def _parse_octet(octet_str: DecOctet) -> int:
  """Convert a decimal octet into an integer.

  Args:
      octet_str: A string, the number to parse.

  Returns:
      The octet as an integer.

  Raises:
      ValueError: if the octet isn't strictly a decimal from [0..255].
  """
  if not octet_str:
    raise ValueError("Empty octet not permitted")
  # Reject non-ASCII digits.
  if not (octet_str.isascii() and octet_str.isdigit()):
    msg = "Only decimal digits permitted in %r"
    raise ValueError(msg % octet_str)
  # We do the length check second, since the invalid character error
  # is likely to be more informative for the user
  if len(octet_str) > 3:
    msg = "At most 3 characters permitted in %r"
    raise ValueError(msg % octet_str)
  # Handle leading zeros as strict as glibc's inet_pton()
  # See security bug bpo-36384
  if octet_str != '0' and octet_str[0] == '0':
    msg = "Leading zeros are not permitted in %r"
    raise ValueError(msg % octet_str)
  # Convert to integer (we know digits are legal)
  octet_int = int(octet_str, 10)
  if octet_int > 255:
    raise ValueError("Octet %d (> 255) not permitted" % octet_int)
  return octet_int


# IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
type IPv4Address = lang(r'{DecOctet}\.{DecOctet}\.{DecOctet}\.{DecOctet}')


def parse_ipv4_address(ip_str: IPv4Address) -> int:
  """Turn the given IP string into an integer for comparison.

  Args:
      ip_str: A string, the IP ip_str.

  Returns:
      The IP ip_str as an integer.

  Raises:
      AddressValueError: if ip_str isn't a valid IPv4 Address.

  """
  if not ip_str:
    raise AddressValueError('Address cannot be empty')

  octets = ip_str.split('.')
  if len(octets) != 4:
    raise AddressValueError("Expected 4 octets in %r" % ip_str)

  try:
    return int.from_bytes(map(_parse_octet, octets), 'big')
  except ValueError as exc:
    raise AddressValueError("%s in %r" % (exc, ip_str)) from None


# h16         = 1*4HEXDIG   ; 16 bits of address represented in hexadecimal
type H16 = lang(r'[0-9A-Fa-f]{1,4}')


def _parse_hextet(hextet_str: H16) -> int:
  """Convert an IPv6 hextet string into an integer.

  Args:
      hextet_str: A string, the number to parse.

  Returns:
      The hextet as an integer.

  Raises:
      ValueError: if the input isn't strictly a hex number from
        [0..FFFF].
  """
  # Reject non-ASCII digits.
  if not set('0123456789ABCDEFabcdef').issuperset(set(hextet_str)):
    raise ValueError("Only hex digits permitted in %r" % hextet_str)
  # We do the length check second, since the invalid character error
  # is likely to be more informative for the user
  if len(hextet_str) > 4:
    msg = "At most 4 characters permitted in %r"
    raise ValueError(msg % hextet_str)
  # Length check means we can skip checking the integer value
  return int(hextet_str, 16)


# IPv6address =                            6( h16 ":" ) ls32
#             /                       "::" 5( h16 ":" ) ls32
#             / [               h16 ] "::" 4( h16 ":" ) ls32
#             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
#             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
#             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
#             / [ *4( h16 ":" ) h16 ] "::"              ls32
#             / [ *5( h16 ":" ) h16 ] "::"              h16
#             / [ *6( h16 ":" ) h16 ] "::"
# ls32        = ( h16 ":" h16 ) / IPv4address   ; least-significant 32 bits of address
type LS32 = lang(r'{H16}:{H16}|{IPv4Address}')
type IPv6Address = lang(
  r'({H16}:){6}{LS32}|::({H16}:){5}{LS32}|{H16}?::({H16}:){4}{LS32}|'
  r'(({H16}:){0,1}{H16})?::({H16}:){3}{LS32}|(({H16}:){0,2}{H16})?::({H16}:){2}{LS32}|'
  r'(({H16}:){0,3}{H16})?::{H16}:{LS32}|(({H16}:){0,4}{H16})?::{LS32}|'
  r'(({H16}:){0,5}{H16})?::{H16}|(({H16}:){0,6}{H16})?::'
)


def parse_ipv6_address(ip_str: IPv6Address) -> int:
  """Turn an IPv6 ip_str into an integer.

  Args:
      ip_str: A string, the IPv6 ip_str.

  Returns:
      An int, the IPv6 address

  Raises:
      AddressValueError: if ip_str isn't a valid IPv6 Address.
  """
  if not ip_str:
    raise AddressValueError('Address cannot be empty')

  parts = ip_str.split(':')

  # An IPv6 address needs at least 2 colons (3 parts).
  _min_parts = 3
  if len(parts) < _min_parts:
    msg = "At least %d parts expected in %r" % (_min_parts, ip_str)
    raise AddressValueError(msg)

  # If the address has an IPv4-style suffix, convert it to hexadecimal.
  if '.' in parts[len(parts) - 1]:
    ipv4_int = parse_ipv4_address(parts.pop())
    parts.append('%x' % ((ipv4_int >> 16) & 0xFFFF))
    parts.append('%x' % (ipv4_int & 0xFFFF))

  # An IPv6 address can't have more than 8 colons (9 parts).
  # The extra colon comes from using the "::" notation for a single
  # leading or trailing zero part.
  _max_parts = 8 + 1
  if len(parts) > _max_parts:
    msg = "At most %d colons permitted in %r" % (_max_parts - 1, ip_str)
    raise AddressValueError(msg)

  # Disregarding the endpoints, find '::' with nothing in between.
  # This indicates that a run of zeroes has been skipped.
  skip_index = -1
  for i in range(1, len(parts) - 1):
    inv(1 <= i < len(parts))
    inv(skip_index == -1 or 1 <= skip_index < i)
    inv(skip_index == parts.index('', 1, len(parts) - 1) if skip_index != -1 else '' not in parts[1:i])
    if not parts[i]:
      if skip_index != -1:
        hint(parts[1:len(parts) - 1].count('') <= 1)
        hint(parts[skip_index] == '')
        # Can't have more than one '::'
        msg = "At most one '::' permitted in %r" % ip_str
        raise AddressValueError(msg)
      skip_index = i

  # parts_hi is the number of parts to copy from above/before the '::'
  # parts_lo is the number of parts to copy from below/after the '::'
  if skip_index != -1:
    # If we found a '::', then check if it also covers the endpoints.
    parts_hi = skip_index
    parts_lo = len(parts) - skip_index - 1
    if not parts[0]:
      parts_hi -= 1
      if parts_hi != 0:
        msg = "Leading ':' only permitted as part of '::' in %r"
        raise AddressValueError(msg % ip_str)  # ^: requires ^::
    if not parts[len(parts) - 1]:
      parts_lo -= 1
      if parts_lo != 0:
        msg = "Trailing ':' only permitted as part of '::' in %r"
        raise AddressValueError(msg % ip_str)  # :$ requires ::$
    parts_skipped = 8 - (parts_hi + parts_lo)
    if parts_skipped < 1:
      msg = "Expected at most %d other parts with '::' in %r"
      raise AddressValueError(msg % (8 - 1, ip_str))
  else:
    # Otherwise, allocate the entire address to parts_hi.  The
    # endpoints could still be empty, but _parse_hextet() will check
    # for that.
    if len(parts) != 8:
      msg = "Exactly %d parts expected without '::' in %r"
      raise AddressValueError(msg % (8, ip_str))
    if not parts[0]:
      msg = "Leading ':' only permitted as part of '::' in %r"
      raise AddressValueError(msg % ip_str)  # ^: requires ^::
    if not parts[len(parts) - 1]:
      msg = "Trailing ':' only permitted as part of '::' in %r"
      raise AddressValueError(msg % ip_str)  # :$ requires ::$
    parts_hi = len(parts)
    parts_lo = 0
    parts_skipped = 0

  try:
    # Now, parse the hextets into a 128-bit integer.
    ip_int = 0
    for i in range(parts_hi):
      ip_int <<= 16
      ip_int |= _parse_hextet(parts[i])
    ip_int <<= 16 * parts_skipped
    for i in range(-parts_lo, 0):
      ip_int <<= 16
      ip_int |= _parse_hextet(parts[len(parts) + i])
    return ip_int
  except ValueError as exc:
    raise AddressValueError("%s in %r" % (exc, ip_str)) from None
