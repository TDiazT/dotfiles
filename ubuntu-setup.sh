#!/bin/sh

PROJECTS_PATH=$HOME/Projects

sudo apt update && sudo apt upgrade -y
sudo apt install git gnome-tweaks curl -y

# Get notes
git clone git@github.com:TDiazT/Notes.git $PROJECTS_PATH/Notes

# Install Emacs 28
echo "INSTALLING EMACS 28"
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt update
sudo apt install emacs28-nativecomp -y

# Setup Chemacs2
echo "INSTALLING CHEMACS 2"
git clone git@github.com:plexus/chemacs2.git $HOME/.emacs.d
echo "((\"default\" . ((user-emacs-directory . \"~/.config/emacs\"))))" > $HOME/.emacs-profiles.el

# Setup Crafted Emacs
CRAFTED_EMACS_PATH=$HOME/.config/emacs

echo "INSTALLING CRAFTED EMACS"
git clone https://github.com/SystemCrafters/crafted-emacs $CRAFTED_EMACS_PATH
mkdir -p $CRAFTED_EMACS_PATH/crafted-emacs
ln -s $PWD/crafted-emacs-early-config.el $CRAFTED_EMACS_PATH/crafted-emacs/early-config.el
ln -s $PWD/crafted-emacs.el $CRAFTED_EMACS_PATH/crafted-emacs/config.el
ln -s $PWD/emacs/modules $CRAFTED_EMACS_PATH/crafted-emacs/custom-modules

## Install GCC to compile SQLite for Org-roam
sudo apt install build-essential -y

# install FiraCode Nerd Font & Iosevka -
echo "[-] Download fonts [-]"
echo "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip"
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip
unzip FiraCode.zip -d $HOME/.local/share/fonts && rm FiraCode.zip
echo "https://github.com/be5invis/Iosevka/releases/download/v15.6.3/ttc-iosevka-aile-15.6.3.zip"
wget https://github.com/be5invis/Iosevka/releases/download/v15.6.3/ttc-iosevka-aile-15.6.3.zip
unzip ttc-iosevka-aile-15.6.3.zip -d $HOME/.local/share/fonts && rm ttc-iosevka-aile-15.6.3.zip
fc-cache -fv
echo "done!"

# Install ripgrep
sudo apt install ripgrep -y

# Install zulip
sudo curl -fL -o /etc/apt/trusted.gpg.d/zulip-desktop.asc \
    https://download.zulip.com/desktop/apt/zulip-desktop.asc
echo "deb https://download.zulip.com/desktop/apt stable main" | \
    sudo tee /etc/apt/sources.list.d/zulip-desktop.list
sudo apt update
sudo apt install zulip

# Install telegram
sudo apt install telegram-desktop -y

# Setup terminal
# Reference : https://andrebrandao.me/articles/terminal-setup-with-zsh-tmux-dracula-theme/
sudo apt install guake -y
echo "Setting up the terminal"
echo "Installing oh-my-zsh"
sudo apt install zsh -y
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

echo "Adding syntax highlighting"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

echo "Adding autosuggestions"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

echo "Adding fuzzy finder (fzf)"
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install

echo "Adding powerlevel10k theme"
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

sed -i 's/ZSH_THEME="robbyrussell"/ZSH_THEME="powerlevel10k\/powerlevel10k"/' $HOME/.zshrc
sed -i 's/plugins=(git)/plugins=(\
    git\
    zsh-syntax-highlighting\
    zsh-autosuggestions\
    tmux)/' $HOME/.zshrc

echo "Installing tmux"
sudo apt install tmux -y
cp $PWD/.tmux.conf $HOME

echo "Installing neovim"
sudo apt install neovim -y
