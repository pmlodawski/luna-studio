require 'colorize'  # What a travesty, indeed. This shall be spelled 'colourise'. Bloody colonies!
require 'open3'


$run_docgen   = false
$run_main     = true
$run_tests    = false
$run_linting  = false
$run_coverage = false

$lastbuild    = Time.now


guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(test|src)/.+\.l?hs$}) do |m|
    if File.mtime(m[0]) < $lastbuild
      puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".green
      puts "File #{m[0]} modified before last build, so rebuilding is unnecessary".green
      puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".green
    else
      $lastbuild = Time.now
      section "haskell file change"
      haskellguard m
    end
  end

  watch(%r{^.*\.tcabal$}) do |m|
    section "tcabal file change"
    haskellguard m
  end

  watch(%r{^runtest.hs$}) do |m|
    section "live tests"
    command "../../../scripts/runhaskell", File.read("runtest.hs")
  end
end



def haskellguard trigger
  puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".yellow
  puts "TRIGGER: #{trigger}".yellow
  puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".yellow
  puts ""

  section "building"
  if command("../../../scripts/compile -j9")

    section "documentation", "cabal haddock --html"  if $run_docgen
    section "tests",         "rm -f luna-typechecker-tests.tix", "../../../dist/bin/libs/luna-typechecker"  if $run_tests

    command "../../../dist/bin/libs/luna-typechecker" if $run_main

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
        coverage_cmd = "hpc markup luna-typechecker-tests --destdir=hpc_report --exclude=Logger #{hpc_excluded_modules} > /dev/null"
        puts "> #{coverage_cmd}"
        command(coverage_cmd)
        puts "Report written to 'hpc_report'"
      end      

      section "linting", "pushd ..; hlint typechecker --report; popd"  if $run_linting
    end
  end
end

def nocolourcodes(arg)
  arg.gsub(/\e\[(\d+)(;\d+)*m/,'')
end

def command(cmd, inp=nil)
  puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".blue
  puts "$ #{cmd}".blue
  puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".blue

  start = Time.now
  stdout, stderr, status = Open3.capture3(cmd, :stdin_data=>inp)
  finish = Time.now
  diff = finish - start

  if status.exitstatus === 0
    puts "  exit code: #{status.exitstatus}".black
  else
    puts "  exit code: #{status.exitstatus}".light_white
  end

  if diff > 5
    diff = "%.2f" % diff
    puts "  exec time: #{diff} sec"
  end

  if stdout
    puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".green
    puts "STDOUT  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".green
    puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".green
    puts stdout
  else
    puts "STDOUT: none  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".green
  end

  if stderr
    puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".red
    puts "STDERR  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".red
    puts "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".red
    puts stderr
  end

  status.exitstatus === 0
end


def section(name, *cmds)
  puts "---------------------------------------------------------------------------------".cyan

  # ascii-arts
  case name
    when "building"      ;  puts "              ____  _  _  __  __    ____  __  __ _   ___\n             (  _ \\/ )( \\(  )(  )  (    \\(  )(  ( \\ / __)\n              ) _ () \\/ ( )( / (_/\\ ) D ( )( /    /( (_ \\ _  _  _\n             (____/\\____/(__)\\____/(____/(__)\\_)__) \\___/(_)(_)(_)".cyan
    when "documentation" ;  puts "    ____   __    ___  _  _  _  _  ____  __ _  ____  __  ____  __  __   __ _\n   (    \\ /  \\  / __)/ )( \\( \\/ )(  __)(  ( \\(_  _)/ _\\(_  _)(  )/  \\ (  ( \\\n    ) D ((  O )( (__ ) \\/ (/ \\/ \\ ) _) /    /  )( /    \\ )(   )((  O )/    /\n   (____/ \\__/  \\___)\\____/\\_)(_/(____)\\_)__) (__)\\_/\\_/(__) (__)\\__/ \\_)__)\n".cyan
    when "tests"         ;  puts "                          ____  ____  ____  ____  ____\n                         (_  _)(  __)/ ___)(_  _)/ ___)\n                           )(   ) _) \\___ \\  )(  \\___ \\\n                          (__) (____)(____/ (__) (____/".cyan
    when "coverage"      ;  puts "                  ___  __   _  _  ____  ____   __    ___  ____ \n                 / __)/  \\ / )( \\(  __)(  _ \\ / _\\  / __)(  __)\n                ( (__(  O )\\ \\/ / ) _)  )   //    \\( (_ \\ ) _)\n                 \\___)\\__/  \\__/ (____)(__\\_)\\_/\\_/ \\___/(____)".cyan
    when "linting"       ;  puts "                      __    __  __ _  ____  __  __ _   ___\n                     (  )  (  )(  ( \\(_  _)(  )(  ( \\ / __)\n                     / (_/\\ )( /    /  )(   )( /    /( (_ \\\n                     \\____/(__)\\_)__) (__) (__)\\_)__) \\___/".cyan
    when "live tests"    ;  puts "                 __    __  _  _  ____    ____  ____  ____  ____\n                (  )  (  )/ )( \\(  __)  (_  _)(  __)/ ___)(_  _)\n                / (_/\\ )( \\ \\/ / ) _)     )(   ) _) \\___ \\  )(\n                \\____/(__) \\__/ (____)   (__) (____)(____/ (__)".cyan
    else puts name.cyan
  end

  puts "---------------------------------------------------------------------------------".cyan

  cmds.map do |c| command(c) end
end