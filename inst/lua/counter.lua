local utils = require 'pandoc.utils'
local a = {}


return {
  {
    Str = function (elem)
      if string.find(elem.text, "^##") then
        if string.find(elem.text, "^##(.+):(.+)") then
          this_grp = string.gsub(elem.text, "^##(.+):(.+)", "%1")
          this_ref = string.gsub(elem.text, "^##(.+):(.+)", "%2")
        else
          this_grp = "all"
          this_ref = string.gsub(elem.text, "^##(.+)", "%1")
        end

        if(a[this_grp] == nil) then
          a[this_grp] = {}
          a[this_grp][this_ref] = 1
        elseif(a[this_grp][this_ref] == nil) then
          num_items = 0
          for k,v in pairs(a[this_grp]) do
            num_items = num_items + 1
          end
          a[this_grp][this_ref] = num_items + 1
        end

        return pandoc.Str(a[this_grp][this_ref])
      end


end,
  }
}

