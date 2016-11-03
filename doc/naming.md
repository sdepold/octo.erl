# Function naming

To help you quickly find octo.erl functions that do what you need, we follow
a very specific scheme of naming our public functions: a verb describing an
action followed by noun(s) describing the object. Boolean checks are using "is"
for a verb and have an adjective after the noun. 

If you need to *create* a *pull request*, you call `octo:create_pull_request`.

If you need to *close* an *issue*, you call `octo:close_issue`.

If you need to *check* if a *pull request* is *merged* already, you call
`octo:is_pull_request_merged`.
