require 'open3'
require 'colorize'  # What a travesty, indeed. This shall be spelled 'colourise'. Bloody colonies!


## Prevent the script to run multiple times in a row for a file saved multiple times
#
# The thing is that `guard` stores actions in FIFO and preforms them all. This global time variable
# is here to short-circuit such cases.

$lastbuild    = Time.now

def lastbuildguard(trigger, &block)
  sleep 1.0/10.0  # arbitrary small sleep to ensure the "save all" is done
  if File.mtime(trigger) < $lastbuild
    puts "File #{trigger} modified before last build, so rebuilding is unnecessary".starsaround.green
  else
    $lastbuild = Time.now
    puts "Building at #{$lastbuild}"
    block.call()
  end
end


## String various operations
#
# For formatting purposes.
# YES, I KNOW that monkey patching of base classes is BAD. However I believe that this is simple enough to not cause
# any harm. If I'm mistaken... well... Sorry :<

class String
  @@linefill_length = 81

  def starfill
    x = self
    if length % 2 != @@linefill_length % 2
      x = x + " "
    end
    x.ljust(@@linefill_length, " *")
  end

  def linefill
    ljust(@@linefill_length, '-')
  end

  def starsaround
    stars = "*".ljust(@@linefill_length, " *")
    replace (stars + "\n" + self + "\n" + stars)
  end

  def starsallaround
    starfill.starsaround
  end

  # This removes the colour-codes from the string. It's handy quite often.
  def nocolourcodes
    gsub(/\e\[(\d+)(;\d+)*m/,'')
  end
end


## Run a command
#
# Run command with all the fanciness of it. Measure execution time, print stdout and stderr in appropriate colours,
# shows the return code. Also passes input (stdin) if provided.

def command(cmd, inp=nil)
  puts "$ #{cmd}".starsaround.blue

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
    puts "STDOUT".starsallaround.green
    puts stdout
  else
    puts "STDOUT: none".starsallaround.green
  end

  unless stderr.empty?
    puts "STDERR".starsallaround.red
    puts stderr
  end

  status.exitstatus === 0
end


## Pretty-printer of sections
#
# Can run commands if passed.
# TODO (feature): could take a block and measure its execution time.

def section(name, *cmds)

  ### Hardcoded ascii-arts
  #
  # Well, I don't like what [`artii`](https://github.com/miketierney/artii) offers - I like best "ANSI shadow"
  # which is not a part of standard distribution...
  #
  # If you want your own ascii-arts then:
  #
  #  a) if you have your favourite font among standard figlet, then just install `artii`
  #
  #         sudo gem install artii
  #
  #     And make the block here to format & center the text.
  #
  #  b) if your favourite is not among what figlet offers, fix that, make a pull request... or hardcode it here

  puts "".linefill.cyan

  case name
    when "building"      ;  puts "              ____  _  _  __  __    ____  __  __ _   ___\n             (  _ \\/ )( \\(  )(  )  (    \\(  )(  ( \\ / __)\n              ) _ () \\/ ( )( / (_/\\ ) D ( )( /    /( (_ \\ _  _  _\n             (____/\\____/(__)\\____/(____/(__)\\_)__) \\___/(_)(_)(_)".cyan
    when "documentation" ;  puts "    ____   __    ___  _  _  _  _  ____  __ _  ____  __  ____  __  __   __ _\n   (    \\ /  \\  / __)/ )( \\( \\/ )(  __)(  ( \\(_  _)/ _\\(_  _)(  )/  \\ (  ( \\\n    ) D ((  O )( (__ ) \\/ (/ \\/ \\ ) _) /    /  )( /    \\ )(   )((  O )/    /\n   (____/ \\__/  \\___)\\____/\\_)(_/(____)\\_)__) (__)\\_/\\_/(__) (__)\\__/ \\_)__)\n".cyan
    when "tests"         ;  puts "                          ____  ____  ____  ____  ____\n                         (_  _)(  __)/ ___)(_  _)/ ___)\n                           )(   ) _) \\___ \\  )(  \\___ \\\n                          (__) (____)(____/ (__) (____/".cyan
    when "coverage"      ;  puts "                  ___  __   _  _  ____  ____   __    ___  ____ \n                 / __)/  \\ / )( \\(  __)(  _ \\ / _\\  / __)(  __)\n                ( (__(  O )\\ \\/ / ) _)  )   //    \\( (_ \\ ) _)\n                 \\___)\\__/  \\__/ (____)(__\\_)\\_/\\_/ \\___/(____)".cyan
    when "linting"       ;  puts "                      __    __  __ _  ____  __  __ _   ___\n                     (  )  (  )(  ( \\(_  _)(  )(  ( \\ / __)\n                     / (_/\\ )( /    /  )(   )( /    /( (_ \\\n                     \\____/(__)\\_)__) (__) (__)\\_)__) \\___/".cyan
    when "live tests"    ;  puts "                 __    __  _  _  ____    ____  ____  ____  ____\n                (  )  (  )/ )( \\(  __)  (_  _)(  __)/ ___)(_  _)\n                / (_/\\ )( \\ \\/ / ) _)     )(   ) _) \\___ \\  )(\n                \\____/(__) \\__/ (____)   (__) (____)(____/ (__)".cyan
    else puts name.cyan
  end

  puts "".linefill.cyan

  cmds.map do |c| command(c) end
end