require 'json'
src2 = "require 'json'\nsrc2 = \"@\"\nsrc2[\"@\"] = src2.to_json[1...-1]\nif src2[/([^)\"1])0/] != nil\n\tsrc2.gsub!(/([^)\"1])0/, '\\12')\nelse\n\tsrc2.gsub!(/([^)\"1])2/, '\\10')\nend\nputs src2\n"
src2["@"] = src2.to_json[1...-1]
if src2[/([^)"1])0/] != nil
	src2.gsub!(/([^)"1])0/, '\12')
else
	src2.gsub!(/([^)"1])2/, '\10')
end
puts src2
