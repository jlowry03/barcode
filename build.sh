#!/usr/bin/env sh
BUILD=${BUILD:-0}
build_rkt(){
    printf "Building %s\n" "$1"
    raco exe --cs "$1"
}
aws(){
    /usr/bin/aws --profile proj $@
}
[ "$BUILD" -eq 1 ] && {
    build_rkt barcode.rkt
    build_rkt widths-to-ppm.rkt

    7z -tzip a function.zip -- \
        README.md barcode widths-to-ppm convert index.mjs node_modules/\
        package.json package-lock.json
    7z -tzip d function.zip -- \
        node_modules/aws-lambda-binary/_examples \
        node_modules/aws-lambda-binary/test \
        node_modules/aws-lambda-binary/CHANGELOG.md
}
du -b function.zip
## figure out how to upload to an already existing function
#aws lambda create-function \
#    --function-name CreateBarcode \
#    --zip-file fileb://function.zip \
#    --handler index.handler \
#    --runtime nodejs18.x \
#    --timeout 10 --memory-size 1024 \
#    --role arn:aws:iam::659586558695:role/lambda-s3-role
