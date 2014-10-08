guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(test|src)/.+\.l?hs$}) do |m|
    puts "\n\n\n--------------------------------------------------------------------------------\n"
    puts "trigger: #{m}"

    # building
    puts "--------------------------------------------------------------------------------\n"
    puts "              ____  _  _  __  __    ____  __  __ _   ___\n             (  _ \\/ )( \\(  )(  )  (    \\(  )(  ( \\ / __)\n              ) _ () \\/ ( )( / (_/\\ ) D ( )( /    /( (_ \\ _  _  _\n             (____/\\____/(__)\\____/(____/(__)\\_)__) \\___/(_)(_)(_)\n"
    puts "--------------------------------------------------------------------------------\n"

    # if system("(cd /Users/konrad/flowbox.io/flowbox/dist/libs/luna/typechecker && exec cabal install -j --reinstall --disable-documentation --bindir=/Users/konrad/flowbox.io/flowbox/dist/bin/libs flowbox-luna-typechecker)")
    if system("~/flowbox.io/flowbox/scripts/compile")
        
        # puts "--------------------------------------------------------------------------------\n"
        # puts "    ____   __    ___  _  _  _  _  ____  __ _  ____  __  ____  __  __   __ _\n   (    \\ /  \\  / __)/ )( \\( \\/ )(  __)(  ( \\(_  _)/ _\\(_  _)(  )/  \\ (  ( \\\n    ) D ((  O )( (__ ) \\/ (/ \\/ \\ ) _) /    /  )( /    \\ )(   )((  O )/    /\n   (____/ \\__/  \\___)\\____/\\_)(_/(____)\\_)__) (__)\\_/\\_/(__) (__)\\__/ \\_)__)\n"
        # puts "--------------------------------------------------------------------------------\n"
        # system("cabal haddock --html")

        # tests
        puts "--------------------------------------------------------------------------------\n"
        puts "                          ____  ____  ____  ____  ____\n                         (_  _)(  __)/ ___)(_  _)/ ___)\n                           )(   ) _) \\___ \\  )(  \\___ \\\n                          (__) (____)(____/ (__) (____/\n"
        puts "--------------------------------------------------------------------------------\n"
        system("rm -f luna-typechecker-tests.tix")
        if system("./dist/dist-sandbox-*/build/luna-typechecker-tests/luna-typechecker-tests --qc-max-success=5000 --print-cpu-time")
        
            # coverage
            puts "--------------------------------------------------------------------------------\n"
            puts "                  ___  __   _  _  ____  ____   __    ___  ____ \n                 / __)/  \\ / )( \\(  __)(  _ \\ / _\\  / __)(  __)\n                ( (__(  O )\\ \\/ / ) _)  )   //    \\( (_ \\ ) _)\n                 \\___)\\__/  \\__/ (____)(__\\_)\\_/\\_/ \\___/(____)\n"
            puts "--------------------------------------------------------------------------------\n"
            system("rm -rf hpc_report")
            hpc_excluded_modules = %w(
                                    Main
                                    Test.Luna.Typechecker.Common
                                    Test.Luna.Typechecker.AST.TypeGen
                                    Test.Luna.TypecheckerSpec
                                    Test.Luna.Typechecker.UnificationSpec
                                    Test.Luna.Typechecker.TypeclassesSpec
                                    Test.Luna.Typechecker.TypeInferenceSpec
                                    Test.Luna.Typechecker.TIMonadSpec
                                    Test.Luna.Typechecker.SubstitutionsSpec
                                    Test.Luna.Typechecker.HasKindSpec
                                    Test.Luna.Typechecker.ContextReductionSpec
                                    Test.Luna.Typechecker.BindingGroupsSpec
                                    Test.Luna.Typechecker.AssumptionsSpec
                                    Test.Luna.Typechecker.AmbiguitySpec
                                    Test.Luna.Typechecker.AST.TypeSpec
                                    Test.Luna.Typechecker.AST.TIDSpec
                                    Test.Luna.Typechecker.AST.SchemeSpec
                                    Test.Luna.Typechecker.AST.PatSpec
                                    Test.Luna.Typechecker.AST.ModuleSpec
                                    Test.Luna.Typechecker.AST.LitSpec
                                    Test.Luna.Typechecker.AST.ExprSpec
                                    Test.Luna.Typechecker.AST.AlternativesSpec
                               ).map{|k| "--exclude=#{k}" }.join(" ")
            system("hpc markup luna-typechecker-tests --destdir=hpc_report #{hpc_excluded_modules} > /dev/null")
            puts "Report written to 'hpc_report'"


            # linting
            puts "--------------------------------------------------------------------------------\n"
            puts "                      __    __  __ _  ____  __  __ _   ___\n                     (  )  (  )(  ( \\(_  _)(  )(  ( \\ / __)\n                     / (_/\\ )( /    /  )(   )( /    /( (_ \\\n                     \\____/(__)\\_)__) (__) (__)\\_)__) \\___/\n"
            puts "--------------------------------------------------------------------------------\n"
            system("pushd ..; hlint typechecker --report; popd")
        end
    end
  end
end 
