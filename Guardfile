require '../../../scripts/utils/guard_tools.rb'

# Preparation:
#
#     sudo gem install guard-shell
#     sudo gem install colorize
#
# Edit this file (has to be named `Guardfile`), then run `guard` from shell. All shall work!


#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Configuration  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$run_docgen   = false
$run_main     = true
$run_tests    = false
$run_linting  = false
$run_coverage = false



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# WATCHED FILES  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(test|src)/.+\.l?hs$}) do |m|
    lastbuildguard(m[0]) do
      section "haskell file change"
      haskellguard m  # refactored out
    end
  end

  watch(%r{^.*\.tcabal$}) do |m|
    lastbuildguard(m[0]) do
      section "tcabal file change"
      haskellguard m  # refactored out
    end
  end

  watch(%r{^runtest.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "live tests"
      command "../../../scripts/runhaskell", File.read("runtest.hs")
    end
  end

  watch(%r{^playground.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "playground file change"
      # command "../../../scripts/runhaskell", File.read("playground.hs")
      command "./playground" if command("ghc playground.hs")
    end
  end

  watch(%r{^src/Maintest.luna$}) do |m|
    lastbuildguard(m[0]) do
      section "Luna file change", "../../../dist/bin/libs/luna-typechecker"
    end
  end
end



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# ACTIONS  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

def haskellguard trigger
  puts "TRIGGER: #{trigger}".starsaround.yellow + "\n"

  section "building"
  if command("../../../scripts/compile -j9")

    section "documentation", "cabal haddock --html"                                                         if $run_docgen
    section "tests",         "rm -f luna-typechecker-tests.tix", "../../../dist/bin/libs/luna-typechecker"  if $run_tests

    command "../../../dist/bin/libs/luna-typechecker"                                                       if $run_main

    if $run_tests and command "../../../dist/bin/libs/luna-typechecker-tests"

      if $run_coverage
        section "coverage", "rm -rf hpc_report"

        hpc_excluded_modules = ((Dir.glob("test/**/*Spec.hs")          # skip all test-spec files
                                    .map { |k| k.gsub("test/", "")     # ...converting path to namespace for HPC
                                                .gsub(".hs","")
                                                .gsub("/",".")
                                         }
                                ) << "Main"                            # and skip "Main", the entrypoint for tests
                               ).map{|k| "--exclude=#{k}" }.join(" ")
        command coverage_cmd
        puts "Report written to 'hpc_report'"
      end

      section "linting", "pushd ..; hlint typechecker --report; popd"  if $run_linting
    end
  end
end


