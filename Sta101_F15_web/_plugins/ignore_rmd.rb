# so that .rmd files are ignored in build and their yaml doesn't get stripped

module RMDignorer
    class RmdCopyer < Jekyll::Generator
        RMD_FOLDER = '_rmd'
        RMD_OUTPUT_DIR = 'rmd'
        def generate(site)
            #FileUtils.cp_r(site.source_dir.RMD_FOLDER, site.dest_dir.RMD_OUTPUT_DIR)
            #FileUtils.cp_r(site.source_dir(RMD_FOLDER), site.dest_dir(RMD_OUTPUT_DIR))
            #FileUtils.cp_r(RMD_FOLDER, RMD_OUTPUT_DIR)
            site.keep_files << RMD_OUTPUT_DIR unless site.keep_files.include?(RMD_OUTPUT_DIR)
        end
    end
end