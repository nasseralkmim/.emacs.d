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
                      " -D Trilinos_CONFIGURE_OPTIONS_FILE=~/.local/src/Trilinos/build/trilinos-fork-master-debug/config.cmake"
                      " -S ~/.local/src/Trilinos/"
                      " -B ~/.local/src/Trilinos/build/trilinos-fork-master-debug/"
                      " && cmake --build  ~/.local/src/Trilinos/build/trilinos-fork-master-debug/ -j32"
                      " && cmake --install ~/.local/src/Trilinos/build/trilinos-fork-master-debug/"))))
