-record(octo_cache_headers, {
          etag          = undefined,
          last_modified = undefined,
          link          = []}).
-record(octo_cache_entry, {
          headers = #octo_cache_headers{},
          result  = undefined}).
