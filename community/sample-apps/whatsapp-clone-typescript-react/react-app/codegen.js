module.exports = {
    "schema": [
        {
            "https://whatsapp-clone-hasura.herokuapp.com/v1alpha1/graphql": {
                "headers": {
                    "x-hasura-admin-secret": "whatsappclone"
                }
            }
        }
    ],
    "documents": [
        "./src/**/*.tsx",
        "./src/**/*.ts"
    ],
    "overwrite": true,
    "generates": {
        "./src/graphql/types.ts": {
            "plugins": [
                "typescript-common",
                "typescript-client"
            ]
        }
    }
};
