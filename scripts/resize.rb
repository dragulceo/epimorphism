#! /usr/bin/env ruby

prefix = "img"
ofs = 0
dim = 1024
`mkdir out`

(Dir["*.jpg"] + Dir["*.png"]).each_with_index do |fn, i|
  outn = "#{prefix}_#{ofs + i}.png"
  `convert #{fn} -resize "#{dim}x#{dim}^" -gravity center -crop #{dim}x#{dim}+0+0 +repage out/#{outn}`
  puts "resizing #{fn} to out/#{outn}"
end

