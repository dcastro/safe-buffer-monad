ghcid:
	ghcid \
		--command "stack ghci \
			--test \
			--bench" \
			--restart package.yaml

ghcid-test:
	ghcid \
		--command "stack ghci \
			--test \
			--bench \
			--ghci-options=-fobject-code" \
		--test main \
		--warnings \
		--restart package.yaml
