require 'colorize'  # What a travesty, indeed. This shall be spelled 'colourise'

guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(test|src)/.+\.l?hs$}) do |m|
    puts "\n\n\n--------------------------------------------------------------------------------".light_magenta
    puts "trigger: #{m}"

    # building
    puts "--------------------------------------------------------------------------------".light_magenta
    puts "              ____  _  _  __  __    ____  __  __ _   ___\n             (  _ \\/ )( \\(  )(  )  (    \\(  )(  ( \\ / __)\n              ) _ () \\/ ( )( / (_/\\ ) D ( )( /    /( (_ \\ _  _  _\n             (____/\\____/(__)\\____/(____/(__)\\_)__) \\___/(_)(_)(_)".cyan
    puts "--------------------------------------------------------------------------------".cyan

    # if system("(cd /Users/konrad/flowbox.io/flowbox/dist/libs/luna/typechecker && exec cabal install -j --reinstall --disable-documentation --bindir=/Users/konrad/flowbox.io/flowbox/dist/bin/libs flowbox-luna-typechecker)")
    if system("~/flowbox.io/flowbox/scripts/compile")
        
        # puts "--------------------------------------------------------------------------------\n"
        # puts "    ____   __    ___  _  _  _  _  ____  __ _  ____  __  ____  __  __   __ _\n   (    \\ /  \\  / __)/ )( \\( \\/ )(  __)(  ( \\(_  _)/ _\\(_  _)(  )/  \\ (  ( \\\n    ) D ((  O )( (__ ) \\/ (/ \\/ \\ ) _) /    /  )( /    \\ )(   )((  O )/    /\n   (____/ \\__/  \\___)\\____/\\_)(_/(____)\\_)__) (__)\\_/\\_/(__) (__)\\__/ \\_)__)\n"
        # puts "--------------------------------------------------------------------------------\n"
        # system("cabal haddock --html")

        # tests
        puts "--------------------------------------------------------------------------------".cyan
        puts "                          ____  ____  ____  ____  ____\n                         (_  _)(  __)/ ___)(_  _)/ ___)\n                           )(   ) _) \\___ \\  )(  \\___ \\\n                          (__) (____)(____/ (__) (____/".cyan
        puts "--------------------------------------------------------------------------------".cyan
        system("rm -f luna-typechecker-tests.tix")
        if system("./dist/dist-sandbox-*/build/luna-typechecker-tests/luna-typechecker-tests --qc-max-success=5000 --print-cpu-time")
        
            # coverage
            puts "--------------------------------------------------------------------------------".cyan
            puts "                  ___  __   _  _  ____  ____   __    ___  ____ \n                 / __)/  \\ / )( \\(  __)(  _ \\ / _\\  / __)(  __)\n                ( (__(  O )\\ \\/ / ) _)  )   //    \\( (_ \\ ) _)\n                 \\___)\\__/  \\__/ (____)(__\\_)\\_/\\_/ \\___/(____)".cyan
            puts "--------------------------------------------------------------------------------".cyan
            system("rm -rf hpc_report")

            hpc_excluded_modules = ((Dir.glob("test/**/*Spec.hs")
                                        .map { |k| k.gsub("test/", "")
                                                    .gsub(".hs","")
                                                    .gsub("/",".")
                                             }
                                    ) << "Main"
                                   ).map{|k| "--exclude=#{k}" }.join(" ")
            coverage_cmd = "hpc markup luna-typechecker-tests --destdir=hpc_report --exclude=Logger #{hpc_excluded_modules} > /dev/null"
            puts "> #{coverage_cmd}"
            system(coverage_cmd)
            puts "Report written to 'hpc_report'"


            # linting
            puts "--------------------------------------------------------------------------------".cyan
            puts "                      __    __  __ _  ____  __  __ _   ___\n                     (  )  (  )(  ( \\(_  _)(  )(  ( \\ / __)\n                     / (_/\\ )( /    /  )(   )( /    /( (_ \\\n                     \\____/(__)\\_)__) (__) (__)\\_)__) \\___/".cyan
            puts "--------------------------------------------------------------------------------".cyan
            system("pushd ..; hlint typechecker --report; popd")
        end
    end
  end
  watch(%r{^runtest.hs$}) do |m|
    puts "--------------------------------------------------------------------------------".cyan
    puts "                 __    __  _  _  ____    ____  ____  ____  ____\n                (  )  (  )/ )( \\(  __)  (_  _)(  __)/ ___)(_  _)\n                / (_/\\ )( \\ \\/ / ) _)     )(   ) _) \\___ \\  )(\n                \\____/(__) \\__/ (____)   (__) (____)(____/ (__)".cyan
    puts "--------------------------------------------------------------------------------".cyan
    IO.popen("../../../scripts/runhaskell ", mode='r+') do |io|
        prog = File.read("runtest.hs")
        puts prog.green
        io.write prog
        io.close_write
        result = io.read
        puts result.yellow
    end 
  end
end 
