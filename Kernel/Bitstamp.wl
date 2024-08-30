(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["EwanDawson`Bitstamp`"];


BitstampAddCredentials::usage = "
	BitstampAddCredentials[SystemCredentials[\"Bitstamp Main Account API Key\"]];
	BitstampAddCredentials[SystemCredentials[\"Bitstamp XYZ Sub-account API Key\"], \"XYZ\"];
";


BitstampAccount::usage = "
	account = BitstampAccount[]
	account = BitstampAccount[\"Sub account name\"]
	
	account[\"OpenOrders\"]
	account[\"AccountBalances\"]
	account[\"BuyLimitOrder\", market, params]
	account[\"SellLimitOrder\", marker, params]
	account[\"CancelOrder\", order]
	account[\"OrderStatus\", order]
	account[\"PlanceOrder\", BitstampOrder]
";


BitstampOrder::usage = "Represents an order in a particular
	Bitstamp market orderbook";


BitstampMarket::usage = "Respesents a market or token pair avaialble
	on Bitstamp";


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*API credentials*)


(* ::Text:: *)
(*Load the Bitstamp API key from the system credentials store. If we're running in the cloud, load from an encrypted cloud object, asking the user for the decryption key.*)


(* ::Input::Initialization:: *)
(*copperleafblows*)
Once[BitstampSwitchAccount["Main"]];


BitstampAuthenticate[creds_SystemCredentialData] :=
    (
        $BitstampApiKey = creds;
        Enclose[
            Confirm[
                BitstampAPIExecute["AccountBalances"];
                Success["Authenticated", <||>]
            ]
        ]
    );


BitstampAccount[] :=
    $BitstampAccount;


BitstampSwitchAccount["Main"] :=
    Module[{
        result =
            BitstampAuthenticate[
                Module[{
                    cred =
                        If[$CloudEvaluation,
                            Decrypt[AuthenticationDialog["Password", 
                                "WindowTitle" -> "Decryption key for Bitstamp API credentials"]["Password"
                                ], PersistentSymbol["BITSTAMP_API_KEY", "Cloud"]]
                            ,
                            SystemCredential["BITSTAMP_API_KEY"]
                        ]
                },
                    If[!MatchQ[cred, _SystemCredentialData],
                        Missing["No Bitstamp API key found"]
                        ,
                        cred
                    ]
                ]
            ]
    },
        Enclose[
            Confirm[$result];
            $BitstampAccount = "Main";
            result
        ]
    ];

BitstampSwitchAccount[account_String] :=
    Module[{result = BitstampAuthenticate[SystemCredential["Bitstamp/"
         <> account <> "/API"]]},
        Enclose[
            Confirm[$result];
            $BitstampAccount = account;
            result
        ]
    ];


Once[$CredentialsStore = <||>];

BitstampAddCredentials[creds : KeyValuePattern[{"Key" -> _String, "Secret"
     -> _String}], account_:None] :=
    $CredentialsStore[account] = Association @ creds;

BitstampAddCredentials[creds_SystemCredentialData, account_:None] :=
    AddCredentials[
        creds //
        Normal //
        First
        ,
        account
    ];


(* ::Subsection:: *)
(*Utility functions for making API calls*)


(* ::Code::Initialization:: *)
GenerateNonce[] :=
    StringJoin @ RandomChoice[CharacterRange @@ Flatten @ ToCharacterCode[
        {"a", "z"}], 36];


(* ::Code::Initialization:: *)
Timestamp[] :=
    IntegerString[UnixTime[] * 1000];


(* ::Code::Initialization:: *)
BitstampAPIBaseRequest[path_List] :=
    <|"Scheme" -> "https", "Domain" -> "www.bitstamp.net", "Path" -> 
        ToString /@ Join[{"", "api", "v2"}, path /. BitstampMarket[left_, right_
        ] :> ToLowerCase[left <> right], {""}]|>;


(* ::Code::Initialization:: *)
SignBitstampAPIRequest[request_HTTPRequest] :=
    Module[{key = $BitstampApiKey["Data"]["Key"], secret = $BitstampApiKey[
        "Secret"], nonce = GenerateNonce[], timestamp = Timestamp[], hmac = ResourceFunction[
        "HMAC"], signature, authHeaders, hasBody = !StringMatchQ[request["Body"
        ], ""]},
        signature = BaseEncode[hmac[StringJoin[Join[{"BITSTAMP", " ",
             key}, Values @ request[{Method, "Domain", "PathString", "QueryString",
             "ContentType"}], {nonce, timestamp, "v2"}, Values @ request[{"Body"}
            ]] /. None -> Nothing], secret], "Base16"];
        authHeaders = {"X-Auth" -> "BITSTAMP " <> key, "X-Auth-Signature"
             -> signature, "X-Auth-Nonce" -> nonce, "X-Auth-Timestamp" -> timestamp,
             "X-Auth-Version" -> "v2"};
        HTTPRequest @ Association[request[{"Scheme", "Domain", "Path",
             Method, "Body", "ContentType"}], "Headers" -> authHeaders]
    ];


(* ::Code::Initialization:: *)
Needs["Toolbelt`"];

BitstampAPIRequest[path_List] :=
    SignBitstampAPIRequest @ HTTPRequest @ Association[BitstampAPIBaseRequest
         @ path, Method -> "GET"];

BitstampAPIRequest[path_List, body_Association] :=
    SignBitstampAPIRequest @
        HTTPRequest @
            Association[
                BitstampAPIBaseRequest @ path
                ,
                Method -> "POST"
                ,
                Sequence[
                    If[Length @ body > 0,
                        {"Body" -> WWWFormURLEncode @ body, "ContentType"
                             -> "application/x-www-form-urlencoded"}
                        ,
                        {"ContentType" -> ""}
                    ]
                ]
            ];

BitstampAPIRequest[path_List, body_List] :=
    BitstampAPIRequest[path, Association @ body]


(* ::Code::Initialization:: *)
NormalizeParamName[name_String] :=
    ToLowerCase[StringReplace[name, a_?LowerCaseQ ~~ b_?UpperCaseQ :>
         a <> "_" <> b]];

NormalizeParamName[any_] :=
    any;

NormalizeParams[params_Association] :=
    Association @ KeyValueMap[
            NormalizeParamName[#1] ->
                Switch[#2,
                    _DateObject,
                        UnixTime[#2]
                    ,
                    _Market,
                        ToString @ #2
                    ,
                    _Quantity,
                        Switch[#1,
                            "Price" | "LimitPrice",
                                Round
                            ,
                            "Amount",
                                ToString[DecimalForm[#, {Infinity, 8}
                                    ]]&
                        ][QuantityMagnitude @ #2]
                    ,
                    True,
                        "true"
                    ,
                    False,
                        "false"
                    ,
                    _,
                        ToString @ #2
                ]&
        ][params];

NormalizeParams[params_List] :=
    NormalizeParams @ Association @ params;


(* ::Subsection:: *)
(*Bitstamp API calls*)


(* ::Code::Initialization:: *)
$ResponseTransformations = Dispatch[{("market" -> market_) :> ("market"
     -> BitstampMarket @@ StringSplit[market, "/"]), ("datetime" -> datetime_
    ) :> ("datetime" -> FromDateString[datetime, TimeZone -> "UTC"]), ("type"
     -> "0") -> ("type" -> "Buy"), ("type" -> "1") -> ("type" -> "Sell"),
     ("amount" -> amount_) :> ("amount" -> Interpreter["Number"][amount]),
     ("price" -> price_) :> ("price" -> Interpreter["Number"][price])}]

BitstampAPIExecute[request_HTTPRequest, f_:Identity] :=
    With[{response = URLRead[request]},
        If[FailureQ @ response,
            Return @ Failure["Bitstamp API Error", <|"MessageTemplate"
                 -> response["Message"]|>]
        ];
        Enclose[f @ ReplaceAll[(key_ -> value_) :> (StringJoin[Capitalize
             /@ StringSplit[key, "_"]] -> value)] @ ReplaceAll[$ResponseTransformations
            ] @ Echo@Import @ ConfirmBy[response, #["StatusCode"] == 200&], Failure["Bitstamp API Error",
             <|"MessageTemplate" -> "`StatusCode`: `StatusCodeDescription`\n`Body`",
             "MessageParameters" -> response[{"StatusCode", "StatusCodeDescription",
             "Body"}]|>]&]
    ];

BitstampAPIExecute["Ticker"] :=
    BitstampAPIExecute[BitstampAPIRequest[{"ticker"}]];

BitstampAPIExecute["PlaceOrder", BitstampOrder[params : KeyValuePattern[
    {"Status" -> "Draft", "Type" -> type : "Buy" | "Sell", "Market" -> market_BitstampMarket,
     "Amount" -> _Quantity, "Price" -> _Quantity}]]] :=
    With[{apiCall = (type /. {"Buy" -> "BuyLimitOrder", "Sell" -> "SellLimitOrder"
        })},
        BitstampOrder[Merge[{params, Prepend["Status"->"Placed"]@Apply[Identity] @ BitstampAPIExecute[
            apiCall, market, KeyTake[params, {"Amount", "Price", "LimitPrice", "DailyOrder",
             "IocOrder", "FocOrder", "MocOrder", "GtdOrder", "ExpireTime", "ClientOrderId"
            }]]}, Last]]
    ];

BitstampAPIExecute["PlaceOrder", BitstampOrder[params_]] :=Enclose[ConfirmMatch[params,KeyValuePattern[
    {"Status" -> "Draft", "Type" -> "Buy" | "Sell", "Market" -> _BitstampMarket,
     "Amount" -> _Quantity, "Price" -> _Quantity}]]];

BitstampAPIExecute["OpenOrders"] :=
    BitstampAPIExecute[BitstampAPIRequest[{"open_orders"}, {}], Map[BitstampOrder
        ]];

BitstampAPIExecute["AccountBalances"] :=
    BitstampAPIExecute @ BitstampAPIRequest[{"account_balances"}, {}];

BitstampAPIExecute["BuyLimitOrder", market_BitstampMarket, parameters_Association
    ] :=
    BitstampAPIExecute[BitstampAPIRequest[{"buy", market}, NormalizeParams
         @ parameters], BitstampOrder];

BitstampAPIExecute["SellLimitOrder", market_BitstampMarket, parameters_Association
    ] :=
    BitstampAPIExecute[BitstampAPIRequest[{"sell", market}, NormalizeParams
         @ parameters], BitstampOrder];

BitstampAPIExecute["CancelOrder", order_BitstampOrder] :=
    BitstampAPIExecute[BitstampAPIRequest[{"cancel_order"}, {"id" -> 
        order["ID"]}], BitstampOrder];

BitstampAPIExecute["UserTransactions", market_BitstampMarket, parameters_Association
    ] :=
    BitstampAPIExecute[BitstampAPIRequest[{"user_transactions", market
        }, NormalizeParams @ parameters]];

BitstampAPIExecute["OrderStatus", order_BitstampOrder] :=
    BitstampAPIExecute[BitstampAPIRequest[{"order_status"}, {"id" -> 
        order["Id"], "omit_transactions" -> "false"}], BitstampOrder];


(* ::Subsection:: *)
(*Bitstamp objects*)


(* ::Code::Initialization:: *)
BitstampMarket /: ToString[BitstampMarket[asset_String, denomination_String
    ]] :=
    ToUpperCase @ StringRiffle[{asset, denomination}, "/"];

BitstampMarket /: MakeBoxes[BitstampMarket[asset_, denomination_], StandardForm
    ] :=
    RowBox[{MakeBoxes[asset, StandardForm], "/", MakeBoxes[denomination,
         StandardForm]}];


(* ::Code::Initialization:: *)
BitstampOrder[data_List] :=
    BitstampOrder[
        data //
        Association //
        Append[#, {"Amount" -> Quantity[#Amount, #Market[[1]]], "Price"
             -> Quantity[#Price, #Market[[2]]]}]&
    ];

BitstampOrder[data_Association]["Association"] :=
    data;

BitstampOrder[data_Association][prop_List] :=
    Module[{value = (Query @@ prop)[data]},
        value
    ];

BitstampOrder[data_Association][prop_] :=
    BitstampOrder[data][{prop}];

BitstampOrder[data_Association][props__] :=
    BitstampOrder[data][{props}]


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
