# Barnacle Fly.io Demo

Lots of inspiration here comes from the
[Nessie cluster example](https://github.com/ckreiling/nessie_cluster_example).

The app is available to view at <https://barnacle-demo.fly.dev/>.

As for the code, you pretty much only need to read the `main` function in
`src/barnacle_demo.gleam`.

## How do I run this?

```sh
fly launch
fly secrets set ERLANG_COOKIE="randomly-generated-cookie"
fly deploy
```

To add more nodes, you can run `fly scale count 4`, for example.

## How does it work?

In terms of setup, what we have is pretty straightforward. `fly.toml` is a
standard Fly.io config file. The only thing of note is the `ERL_AFLAGS` env
var, which sets the node name and cookie, and ensures we're using IPv6. The
cookie is set using a Fly secret.

Instead of calling `gleam run` directly in our `Dockerfile`, we have a
`docker-entrypoint.sh` that expands environment variables with things like
our Fly image reference and machine name, which are only available at runtime.

We can then use environment variables to configure Barnacle's DNS strategy to
query `${FLY_APP_NAME}.internal`, which is your app's internal hostname.

## What's all this code?

A lot of it is a Lustre frontend using server components to show which nodes
are currently in the cluster. You can probably ignore it, or learn about Luster
server components from [Isaac's YouTube video](https://youtu.be/bzvYJHRrin0?si=l3eMZgT-SzTCRefu).
