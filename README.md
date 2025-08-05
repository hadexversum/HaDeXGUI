# HaDeXGUI

HaDeXGUI is a dedicated package containing GUI for the functionalities from [HaDeX2](https://github.com/hadexversum/HaDeX2) package.
Therefore, to install and run HaDeXGUI, it is necessary to install the correct version of the HaDeX package.

```
remotes::install_github("https://github.com/hadexversum/HaDeX2")
remotes::install_github("https://github.com/hadexversum/HaDeXGUI")
```

Once all the dependencies are installed, the application is ready to run.

```
options(golem.app.prod = TRUE)
HaDeXGUI::run_app()
```

The other way to use HaDeXGUI is to call the HaDeX::HaDeX_GUI() function from the HaDeX2 package.
