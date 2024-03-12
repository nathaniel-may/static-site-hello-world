# Callback Server

This server is supposed to be a dead-simple way to write strings to files from the browser.

## But why

Because why not write even more build tooling.

Halogen apps start with an empty html file, then renders it from the bundled js code. Which means there's a lot of leadup to the first contentful paint (FCP) with multiple network calls and latency from running the JS. I don't really notice on my devices, but lighthouse certainly does and I like making lighthouse have green numbers.

Great- just pregenerate the initial html and leave it sitting there for the server to have in one http call problem solved. Except in order to generate that, halogen has to actually be run in the browser and browsers can't just write files. There's two choices: use a data uri to trigger a download of a string to a file or make a post call with JS to a running server. Browsers try to protect you against data uris in different ways so I don't trust that'll work exactly the same in future browsers. Also it would require some level of human intervention. With a server it'll always work and I can easily make sure the file goes to the right place with automation.