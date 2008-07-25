# Update the Windows icon file using NetPBM.  Yes, we know that NetPBM is
# getting pretty flaky on some platforms these days--you may need to run
# this more than once.
namespace :halyard do 
  PBM_ICON_FILES=%w(icon16.pnm icon16.pgm icon32.pnm icon32.pgm).map do |name|
    "local/branding/#{name}"
  end
  
  desc 'Update the Win32 icon file using NetPBM'
  task :icon => 'local/branding/application.ico'
  
  file 'local/branding/application.ico' => PBM_ICON_FILES do |t|
    srcs = PBM_ICON_FILES.join(' ')
    sh "ppmtowinicon -andpgms -truetransparent #{srcs} > #{t.name}"
  end
  
  # Create a PNM file from a PNG.  Contains the image data.
  rule '.pnm' => '.png' do |t|
    sh "pngtopnm #{t.source} > #{t.name}"
  end
  
  # Create a PGM file from a PNG.  Contains the mask.
  rule '.pgm' => '.png' do |t|
    sh "pngtopnm -alpha #{t.source} > #{t.name}"
  end
end
