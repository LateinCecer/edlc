#!/bin/sh
set -e
EDL_HOME="${HOME}/.edl"
if [ -d "$EDL_HOME" ]; then
  echo "found edl installation at $EDL_HOME. Reinstalling..."
  rm -rf "$EDL_HOME"
fi

mkdir "${HOME}/.edl"
mkdir "${HOME}/.edl/bin"
mkdir "${HOME}/.edl/include"
# build the doc server binary and install it
# (note: cargo-leptos writes the site output to <workspace>/target/site for
# both debug and release builds, so there is no target/release/site to copy)
cargo leptos build --release
cp ../target/release/edlc_doc_server "${HOME}/.edl/bin/edl_docs"
cp -r ../target/site "${HOME}/.edl/site"
# add environment shell script
touch "${HOME}/.edl/env"
cat << EOF > "$EDL_HOME/env"
#!/bin/sh
# edl shell setup
# affix colons on either side of \$PATH to simplify matching (inspired by cargo shell setup)
case ":\${PATH}:" in
  *:"\$HOME/.edl/bin":*)
    ;;
  *)
    export PATH="\$HOME/.edl/bin:\$PATH"
    ;;
esac
# where the installed doc server should look for its Leptos site assets
export EDL_DOC_SITE="\$HOME/.edl/site"
EOF
chmod +x "$EDL_HOME/env"
if grep -q -F '. "$HOME/.edl/env"' "${HOME}/.bashrc"; then
  echo "environment is already installed to .bashrc"
else
  echo '. "$HOME/.edl/env"' >> "$HOME/.bashrc"
fi
# do the same with .profile
if grep -q -F '. "$HOME/.edl/env"' "${HOME}/.profile"; then
  echo "environment is already installed to .profile"
else
  echo '. "$HOME/.edl/env"' >> "$HOME/.profile"
fi

echo ""
echo ""
echo "Installation completed successfully!"
echo "To use the doc server, restart your shell (or source $EDL_HOME/env), then run:"
echo "  edl_docs serve --db /path/to/docs.db"
