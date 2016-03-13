-record(octo_cache_entry_key, {
          function  = undefined,
          arguments = []
         }).
-record(octo_cache_headers, {
          etag          = undefined,
          last_modified = undefined}).
-record(octo_cache_entry, {
          headers = #octo_cache_headers{},
          result  = undefined}).
