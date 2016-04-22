-include_lib("jsonerl/src/jsonerl.hrl").
-include("octo_pull_request.hrl").
-include("octo_commit.hrl").
-include("octo_file.hrl").
-include("octo_reference.hrl").
-include("octo_cache.hrl").
-include("octo_organization.hrl").

-record(octo_error, {
  message,
  documentation_url}).
