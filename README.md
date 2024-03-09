# [WIP] static-site-hello-world
Web dev is hard. This stack gives you more tools to deal with the suck. Mostly just a template for me to copy when I make new sites.

## Wat

![](diagram.png)

## Build tools for your build tools
- JavaScript built from PureScript source
- CSS built from tailwindcss classnames in PureScriptSource
- Initial HTML file built from running PureScript module
- Responsive-sized .webp images built from source png or jpg images
- All those build commands are complicated, and the hope that this endless line of bash still works dwindles quickly, so in comes scripts.hs which (optionally) tests itself on every run.
- The install situation for all this is absurd, so we use a nix derivation to never think about it again even when we move machines.

## TODO
- 