namespace :halyard do 
  desc 'Run all test cards under tests/'
  task :test do
    halyard_command "(command-line-test-driver)"
  end
end
