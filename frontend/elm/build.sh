ELM_ENV=$1

if [[ $ELM_ENV == "--prod" ]]
then
  ELM_MAKE_FLAG="--optimize"
elif [[ $ELM_ENV == "--dev" ]]
then
  ELM_MAKE_FLAG="--debug"
else
  ELM_MAKE_FLAG=""
fi

elm make src/Main.elm $ELM_MAKE_FLAG --output=../../../webroot/scripts/elm.js
