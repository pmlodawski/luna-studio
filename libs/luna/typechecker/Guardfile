guard :shell, :version => 2, :cli => "--color" do
  watch(%r{(test|src)/.+\.l?hs$}) do |m|
    puts "\n\n\nBuilding…\n"
    `~/flowbox.io/flowbox/scripts/compile`
    puts "\n\n\nTests…\n"
    `./dist/dist-sandbox-*/build/flowbox-luna-typechecker-tests/flowbox-luna-typechecker-tests`
  end
end 
