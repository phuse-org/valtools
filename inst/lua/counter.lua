local utils = require 'pandoc.utils'
local counter = 0
local a = {}

function Code (code)
  if string.find(utils.stringify(code), '@@') then
    ref_id = string.gsub(utils.stringify(code), "@@(.)", "\\%1")
    if(a[ref_id] == nil) then
      counter = counter + 1
      ref_id =  string.gsub(utils.stringify(code), "@@(.)", "\\%1")
      a[ref_id] = counter

    end
  end

  return pandoc.Str(a[ref_id])

end
