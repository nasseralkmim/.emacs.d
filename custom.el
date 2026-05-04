;;; -*- lexical-binding: t -*-
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(modus-themes-search-current ((((class color) (min-colors 256)) :background "#f38ba8" :foreground "#11111b")))
 '(modus-themes-search-lazy ((((class color) (min-colors 256)) :background "#3e5768" :foreground "#cdd6f5")))
 '(newsticker-extra-face ((((class color) (min-colors 256)) :foreground "#9399b2" :height 0.8 :slant italic)))
 '(newsticker-feed-face ((((class color) (min-colors 256)) :foreground "#f38ba8" :height 1.2 :weight bold)))
 '(newsticker-treeview-face ((((class color) (min-colors 256)) :foreground "#cdd6f4")))
 '(newsticker-treeview-selection-face ((((class color) (min-colors 256)) :background "#3e5768" :foreground "#cdd6f5"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "d86217004d482042e69272ed4573ae3cfc559161")
 '(package-selected-packages
   '(apptainer-mode ascii-latex-preview edraw eglot-booster eglot-inactive-regions
                    flymake-ruff gt indent-bars jinx kirigami nerd-icons-completion
                    nerd-icons-dired org-mode org-ql pdd preview-auto pueue ultra-scroll
                    winpulse))
 '(package-vc-selected-packages
   '((ghostel :url "https://github.com/dakra/ghostel")
     (org-mode :url "https://code.tecosaur.net/tec/org-mode.git" :branch "dev")
     (ascii-latex-preview :url "https://github.com/nasseralkmim/ascii-latex-preview")
     (org-xopp :url "https://github.com/mahmoodsh36/org-xopp")
     (gptel-quick :url "https://github.com/karthink/gptel-quick")
     (apptainer-mode :url "https://github.com/jrgant/apptainer-mode")
     (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
     (edraw :url "https://github.com/misohena/el-easydraw")
     (vterm-anti-flicker-filter :url
                                "https://github.com/martinbaillie/vterm-anti-flicker-filter")))
 '(safe-local-variable-values
   '((compile-command concat
                      "cmake --fresh -S ~/.local/src/mpFEM/ -B ~/.local/src/mpFEM/ && "
                      "cmake --build ~/.local/src/mpFEM/ -j32 && "
                      "ctest --output-on-failure --test-dir ~/.local/src/mpFEM/ -j32")
     (eval add-hook 'after-save-hook #'org-babel-tangle)
     (compile-command concat
                      "source ~/miniconda3/etc/profile.d/conda.sh && conda activate edelweissfe && "
                      "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build "
                      "python setup.py build_ext -i --force")
     (compile-command concat
                      "source ~/miniconda3/etc/profile.d/conda.sh && activate edelweissfe && "
                      "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build "
                      "python setup.py build_ext -i --force")
     (compile-command concat "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build "
                      "conda run -n edelweissfe " "python setup.py build_ext -i --force")
     (compile-command concat "conda init bash && conda activate edelweissfe && "
                      "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build "
                      "python setup.py build_ext -i --force")
     (compile-command concat "conda activate edelweissfe && "
                      "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build "
                      "python setup.py build_ext -i --force")
     (compile-command concat "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build "
                      "~/miniconda3/envs/edelweissfe/bin/python setup.py build_ext -i --force")
     (compile-command concat "MARMOT_INSTALL_DIR=~/.local/src/Marmot/build"
                      "~/miniconda3/envs/edelweissfe/bin/python setup.py build_ext -i --force")
     (delete-by-moving-to-trash)
     (compile-command concat
                      "cmake --fresh -DCMAKE_INSTALL_PREFIX=~/.local/src/Marmot/build-debug "
                      "-DCMAKE_BUILD_TYPE=Debug -S ~/.local/src/Marmot "
                      "-B ~/.local/src/Marmot/build-debug/ && "
                      "cmake --build  ~/.local/src/Marmot/build-debug -j32 && "
                      "cmake --install ~/.local/src/Marmot/build-debug && "
                      "ctest --output-on-failure --test-dir ~/.local/src/Marmot/build-debug")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/dev-release/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/Trilinos/dev-release/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/dev-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/dev-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/dev-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/master-epetra-release/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/Trilinos/master-epetra-release/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/master-epetra-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/master-epetra-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/master-epetra-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/master[epetra]-release/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/Trilinos/master[epetra]-release/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/master[epetra]-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/master[epetra]-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/master[epetra]-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/master[epetra]-release/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/master[epetra]-release/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/master[epetra]-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/master[epetra]-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/master[epetra]-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/dev-debug/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/Trilinos/dev-debug/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/dev-debug/"
                      " && cmake --build  ~/.local/src/Trilinos/build/dev-debug/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/dev-debug/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked-debug/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/dev-muelu-add-userdata-for-blocked-debug/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked-debug/"
                      " && cmake --build  ~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked-debug/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked-debug/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/dev-muelu-add-userdata-for-blocked/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked/"
                      " && cmake --build  ~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/dev-muelu-add-userdata-for-blocked/")
     (compile-command
      "cmake --fresh -DCMAKE_INSTALL_PREFIX=~/.local/src/Marmot/build-debug "
      "-DCMAKE_BUILD_TYPE=Debug -S ~/.local/src/Markmot "
      "-B ~/.local/src/Marmot/build-debug/ && "
      "cmake --build  ~/.local/src/Marmot/build-debug -j32 && "
      "cmake --install ~/.local/src/Marmot/build-debug && "
      "ctest --output-on-failure --test-dir ~/.local/src/Marmot/build-debug")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/master-release/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/master-release/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/master-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/master-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/master-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/intrepid2-develop-debug/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/intrepid2-develop-debug/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/intrepid2-develop-debug/"
                      " && cmake --build  ~/.local/src/Trilinos/build/intrepid2-develop-debug/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/intrepid2-develop-debug/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/intrepid2-develop-release/config.cmake"
                      " -D CMAKE_INSTALL_PREFIX=/home/nasser/.opt/intrepid2-develop-release/"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/intrepid2-develop-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/intrepid2-develop-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/intrepid2-develop-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/intrepid2-develop-release/config.cmake"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/intrepid2-develop-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/intrepid2-develop-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/intrepid2-develop-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/74d609-release/config.cmake"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/74d609-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/74d609-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/74d609-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/trilinos-fork-master-release/config.cmake"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/trilinos-fork-master-release/"
                      " && cmake --build  ~/.local/src/Trilinos/build/trilinos-fork-master-release/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/trilinos-fork-master-release/")
     (compile-command concat "cmake" " --fresh"
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/trilinos-fork-master-debug/config.cmake"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/trilinos-fork-master-debug/"
                      " && cmake --build  ~/.local/src/Trilinos/build/trilinos-fork-master-debug/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/trilinos-fork-master-debug/"))))
