ghcid:
	ghcid \
		--command "stack ghci \
			--test \
			--bench"

ghcid-test:
	ghcid \
		--command "stack ghci \
			--test \
			--bench \
			--ghci-options=-fobject-code" \
		--test main \
		--warnings
