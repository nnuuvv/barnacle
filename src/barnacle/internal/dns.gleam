import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/string

@external(erlang, "barnacle_ffi", "lookup_a")
fn lookup_a(hostname: String) -> List(#(Int, Int, Int, Int))

@external(erlang, "barnacle_ffi", "lookup_aaaa")
fn lookup_aaaa(
  hostname: String,
) -> List(#(Int, Int, Int, Int, Int, Int, Int, Int))

type IpAddress {
  IpV4(Int, Int, Int, Int)
  IpV6(Int, Int, Int, Int, Int, Int, Int, Int)
}

fn lookup_ip(hostname: String) -> List(IpAddress) {
  let a_records =
    lookup_a(hostname)
    |> list.map(fn(ip) {
      let #(a, b, c, d) = ip
      IpV4(a, b, c, d)
    })

  let aaaa_records =
    lookup_aaaa(hostname)
    |> list.map(fn(ip) {
      let #(a, b, c, d, e, f, g, h) = ip
      IpV6(a, b, c, d, e, f, g, h)
    })

  list.concat([a_records, aaaa_records])
}

fn format_ip_address(ip: IpAddress) -> String {
  case ip {
    IpV4(a, b, c, d) -> {
      [a, b, c, d]
      |> list.map(int.to_string)
      |> string.join(".")
    }
    IpV6(a, b, c, d, e, f, g, h) -> {
      [a, b, c, d, e, f, g, h]
      |> list.map(fn(value) {
        case value {
          0 -> ""
          _ -> int.to_base16(value)
        }
      })
      |> string.join(":")
      |> string.lowercase
    }
  }
}

pub fn discover_nodes(
  basename: String,
  hostname_query: String,
) -> Result(List(atom.Atom), Nil) {
  lookup_ip(hostname_query)
  |> list.map(fn(ip) {
    let ip_string = format_ip_address(ip)
    { basename <> "@" <> ip_string } |> atom.create_from_string
  })
  |> Ok
}
