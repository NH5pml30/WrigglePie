require 'json'
src0 = "require 'json'\nsrc0 = \"@\"\nsrc0[\"@\"] = src0.to_json[1...-1]\nif src0[/([^)\"1])0/] != nil\n\tsrc0.gsub!(/([^)\"1])0/, '\\12')\nelse\n\tsrc0.gsub!(/([^)\"1])2/, '\\10')\nend\nputs src0\n"
src0["@"] = src0.to_json[1...-1]
if src0[/([^)"1])0/] != nil
	src0.gsub!(/([^)"1])0/, '\12')
else
	src0.gsub!(/([^)"1])2/, '\10')
end
puts src0
