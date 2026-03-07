;;; -*- lexical-binding: t -*-
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(activities apptainer-mode ascii-latex-preview auctex biblio breadcrumb calfw cape
                cdlatex citar-embark clipetty copilot csv-mode dape diff-hl dimmer
                dired-ranger dired-subtree dslide ebdb edraw eglot-booster
                eglot-inactive-regions embark-consult evil flash flymake-ruff
                gptel-magit gptel-quick gt helpful highlight-doxygen hl-todo iedit
                indent-bars jinx key-chord kkp ledger-mode marginalia meow moc
                nerd-icons-completion nerd-icons-dired nerd-icons-ibuffer nyan-mode
                orderless org-appear org-contrib org-drill org-gcal org-modern
                org-rainbow-tags org-xopp orgit-forge ox-gfm popper pueue python-black
                pyvenv rainbow-delimiters rainbow-mode shell-maker standard-themes
                treesit-auto treesit-fold ultra-scroll vertico vterm-anti-flicker-filter
                wgrep winpulse yaml-mode))
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
