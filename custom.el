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
 '(safe-local-variable-values
   '((compile-command concat "cmake" " --fresh"
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
