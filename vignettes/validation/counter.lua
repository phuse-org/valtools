local utils = require 'pandoc.utils'
local counter = 0
local a = {}






return {
  {
    Str = function (elem)
 --[[     if string.find(elem.text, "@vt%-") then
        ref_id = string.gsub(elem.text, "@vt%-(.)", "%1")
        if(a[ref_id] == nil) then
          counter = counter + 1
          a[ref_id] = counter
        end
        return pandoc.Str(a[ref_id])
        end
--]]

if string.find(elem.text, "@+(.)") then
        ref_id = string.gsub(elem.text, "@+(.)", "%1")
        if(a[ref_id] == nil) then
          counter = counter + 1
          a[ref_id] = counter
        end
        return pandoc.Str(counter)
        end

    end,
    Code = function  (code)
      if string.find(utils.stringify(code), '@@') then

      ref_id = string.gsub(utils.stringify(code), "@@(.)", "%1")

      if(a[ref_id] == nil) then
        counter = counter + 1
        a[ref_id] = counter
      end
      return pandoc.Str(a[ref_id])
    end


end,
  }
}

