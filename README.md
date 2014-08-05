how-many-lines-in-project
=========================

Calculate how many lines are there in your project.

Introduction
------------

This library provides a method for quickly calculating how many lines in a given project.  
It requires `find-file-in-project`.

Installation
------------

It is recommended installed by the ELPA package system. You should install it by `M-x:` with `package-install: how-many-lines-in-project`.

Usage
-----

`M-x: how-many-lines-in-project`


You may need to config the variable `ffip-patterns` in `find-file-in-project`.  
For example:

    (eval-after-load 'find-file-in-project
      (progn
        (setq ffip-patterns (append '("*.scala" "*.sbt") ffip-patterns))
        (setq ffip-patterns (append '("*.scm" "*.ss") ffip-patterns))))
        
or  

    (eval-after-load 'how-many-lines-in-project
      (progn
        (setq ffip-patterns (append '("*.scala" "*.sbt") ffip-patterns))
        (setq ffip-patterns (append '("*.scm" "*.ss") ffip-patterns))))
