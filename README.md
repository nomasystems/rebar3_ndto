# rebar3_ndto
![rebar3_ndto ci](https://github.com/nomasystems/rebar3_ndto/actions/workflows/ci.yml/badge.svg)

A rebar3 plugin for the automatic generation of [`ndto`](https://github.com/nomasystems/ndto) modules.

## Setup

To use `rebar3_ndto` in your project, you need to declare it in the `plugins` section of your `rebar.config` file:
```erl
{plugins, [
    {rebar3_ndto, {git, "https://github.com/nomasystems/rebar3_ndto.git", {branch, "main"}}}
]}.
```

You can optionally automatize its usage using `provider_hooks`:
```erl
{provider_hooks, [
    {pre, [
        {compile, {ndto, compile}},
        {clean, {ndto, clean}}
    ]}
]}.
```

Finally, configure the plugin's option under the `ndto` key:
```erl
{ndto, [
    {output_dir, "foo/foo_output_dir/"}, % defaults to "_gen/dtos"
    {parser, foo_parser}, % defaults to ndto_parser_json_schema_draft_04
    {specs, [
        "foo/specs/foo.json", % uses default options
        {"foo/bar/specs/foo_bar.json", [
            {output_dir, "foo/bar/foo_bar_ouput_dir/"}, % overrides default output_dir
            {parser, foo_bar_parser} % overrides default parser
        ]}
    ]}
]}.
```


## License

`rebar3_ndto` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.
