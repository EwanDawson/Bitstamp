(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["EwanDawson`Bitstamp`"];


BitstampAddCredentials::usage = "
	BitstampAddCredentials[SystemCredentials[\"Bitstamp XYZ Sub-account API Key\"], \"XYZ\"];
";


Bitstamp::usage = "
	Invoke Bitstamp public API endpoints.
	
	Examples:
	Bitstamp[\"Ticker\"] gets the ticker prices
";


BitstampAccount::usage = "
	Invoke Bitstamp private API funtions, using the keys associated with an account that's already been registered using BitstampAddCredentials[].

	account = BitstampAccount[\"Account name\"]
	
	account[\"OpenOrders\"]
	account[\"AccountBalances\"]
	account[\"BuyLimitOrder\", market, params]
	account[\"SellLimitOrder\", marker, params]
	account[\"CancelOrder\", order]
	account[\"OrderStatus\", order]
	account[\"PlanceOrder\", BitstampOrder]
";


BitstampAccounts::usage = "
	BitstampAccount[] gives the list of accounts for which creditials have been added.
";


BitstampLastPrice::usage = "
	BitstampLastPrice[BitstampMarket[\"BTC\",\"GBP\"]] gives the latest BTC/GBP price. The result is cached for 5 seconds.
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


Once[$BitstampCredentialStore = <||>];

BitstampAddCredentials[creds : KeyValuePattern[{"Key" -> _String, "Secret"
     -> _String}], account_String] :=
    (
        $BitstampCredentialStore[account] = Association @ creds;
        Success["CredentialAdded", <|"MessageTemplate" -> "Key for account `account` added.",
             "MessageParameters" -> <|"account" -> account|>|>]
    );

BitstampAddCredentials[creds_SystemCredentialData, account_String] :=
    BitstampAddCredentials[
        creds //
        Normal //
        First
        ,
        account
    ];

BitstampAccounts[] :=
    Keys[$BitstampCredentialStore];


(* ::Subsection:: *)
(*Utility functions for making API calls*)


GenerateNonce[] :=
    StringJoin @ RandomChoice[CharacterRange @@ Flatten @ ToCharacterCode[
        {"a", "z"}], 36];


Timestamp[] :=
    IntegerString[UnixTime[] * 1000];


BitstampAPIBaseRequest[path_List] :=
    <|"Scheme" -> "https", "Domain" -> "www.bitstamp.net", "Path" -> 
        ToString /@ Join[{"", "api", "v2"}, path /. BitstampMarket[left_, right_
        ] :> ToLowerCase[left <> right], {""}]|>;


SignBitstampAPIRequest[request_HTTPRequest, accountName_String] :=
    Module[{key = $BitstampCredentialStore[accountName]["Key"], secret = $BitstampCredentialStore[
        accountName]["Secret"], nonce = GenerateNonce[], timestamp = Timestamp[
        ], hmac = ResourceFunction["HMAC"], signature, authHeaders, hasBody =
         !StringMatchQ[request["Body"], ""]},
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


Needs["Toolbelt`"];

BitstampAPIRequest[path_List, accountName_String] :=
    SignBitstampAPIRequest[HTTPRequest @ Association[BitstampAPIBaseRequest
         @ path, Method -> "GET"], accountName];

BitstampAPIRequest[path_List, body_Association, accountName_String] :=
    With[{
        request =
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
                ]
    },
        Switch[accountName,
            "",
                request
            ,
            _,
                SignBitstampAPIRequest[request, accountName]
        ]
    ];

BitstampAPIRequest[path_List, body_List, accountName_String] :=
    BitstampAPIRequest[path, Association @ body, accountName];


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


$ResponseTransformations = Dispatch[{("market" -> market_) :> ("market"
     -> BitstampMarket @@ StringSplit[market, "/"]), ("datetime" -> datetime_
    ) :> ("datetime" -> FromDateString[datetime, TimeZone -> "UTC"]), ("type"
     -> "0") -> ("type" -> "Buy"), ("type" -> "1") -> ("type" -> "Sell"),
     ("amount" -> amount_) :> ("amount" -> Interpreter["Number"][amount]),
     ("price" -> price_) :> ("price" -> Interpreter["Number"][price])}];

BitstampAPIExecute[path_List, params_, accountName_String, f_:Identity
    ] :=
    With[{response = URLRead[BitstampAPIRequest[path, NormalizeParams
         @ params, accountName]]},
        If[FailureQ @ response,
            Return @ Failure["Bitstamp API Error", <|"MessageTemplate"
                 -> response["Message"]|>]
        ];
        Enclose[f @ ReplaceAll[(key_ -> value_) :> (StringJoin[Capitalize
             /@ StringSplit[key, "_"]] -> value)] @ ReplaceAll[$ResponseTransformations
            ] @ Import @ ConfirmBy[response, #["StatusCode"] == 200&], Failure["Bitstamp API Error",
             <|"MessageTemplate" -> "`StatusCode`: `StatusCodeDescription`\n`Body`",
             "MessageParameters" -> response[{"StatusCode", "StatusCodeDescription",
             "Body"}]|>]&]
    ];

BitstampPublicAPIExecute[path_List, params_, f_:Identity] :=
    BitstampAPIExecute[path, params, "", f];

Bitstamp["Ticker"] :=
    BitstampPublicAPIExecute[{"ticker"}, {}];

BitstampAccount[accountName_String]["PlaceOrder", BitstampOrder[params
     : KeyValuePattern[{"Status" -> "Draft", "Type" -> type : "Buy" | "Sell",
     "Market" -> market_BitstampMarket, "Amount" -> _Quantity, "Price" ->
     _Quantity}]]] :=
    With[{apiCall = (type /. {"Buy" -> "buy", "Sell" -> "sell"})},
        BitstampOrder[Merge[{params, Prepend["Status" -> "Placed"] @ 
            Apply[Identity] @ BitstampAPIExecute[{apiCall, market}, KeyTake[params,
             {"Amount", "Price", "LimitPrice", "DailyOrder", "IocOrder", "FocOrder",
             "MocOrder", "GtdOrder", "ExpireTime", "ClientOrderId"}], accountName,
             BitStampOrder]}, Last]]
    ];

BitstampAccount[accountName_String]["PlaceOrder", BitstampOrder[params_
    ]] :=
    Enclose[ConfirmMatch[params, KeyValuePattern[{"Status" -> "Draft",
         "Type" -> "Buy" | "Sell", "Market" -> _BitstampMarket, "Amount" -> _Quantity,
         "Price" -> _Quantity}]]];

BitstampAccount[accountName_String]["OpenOrders"] :=
    BitstampAPIExecute[{"open_orders"}, {}, accountName, Map[BitstampOrder
        ]];

BitstampAccount[accountName_String]["AccountBalances"] :=
    BitstampAPIExecute[{"account_balances"}, {}, accountName];

BitstampAccount[accountName_String]["BuyLimitOrder", market_BitstampMarket,
     parameters_Association] :=
    BitstampAPIExecute[{"buy", market}, NormalizeParams @ parameters,
         accountName, BitstampOrder];

BitstampAccount[accountName_String]["SellLimitOrder", market_BitstampMarket,
     parameters_Association] :=
    BitstampAPIExecute[{"sell", market}, NormalizeParams @ parameters,
         accountName, BitstampOrder];

BitstampAccount[accountName_String]["CancelOrder", order_BitstampOrder
    ] :=
    BitstampAPIExecute[{"cancel_order"}, {"id" -> order["ID"]}, accountName,
         BitstampOrder];

BitstampAccount[accountName_String]["UserTransactions", market_BitstampMarket,
     parameters_Association] :=
    BitstampAPIExecute[{"user_transactions", market}, NormalizeParams
         @ parameters, accountName];

BitstampAccount[accountName_String]["OrderStatus", order_BitstampOrder
    ] :=
    BitstampAPIExecute[{"order_status"}, {"id" -> order["Id"], "omit_transactions"
         -> "false"}, accountName, BitstampOrder];

BitstampLastPrice[market_BitstampMarket] :=
    CreateCachedValue[
        "BitstampLastPrice/" <> ToString[market]
        ,
        5
        ,
        Function[{},
            Bitstamp["Ticker"] //
            SelectFirst[Lookup[#, "Pair"] == ToString @ market&] //
            Lookup["Last"] //
            FromDigits //
            Quantity[#, market[[2]]]&
        ]
    ][];


(* ::Subsection:: *)
(*Bitstamp objects*)


BitstampMarket /: ToString[BitstampMarket[asset_String, denomination_String
    ]] :=
    ToUpperCase @ StringRiffle[{asset, denomination}, "/"];

BitstampMarket /: MakeBoxes[BitstampMarket[asset_, denomination_], StandardForm
    ] :=
    RowBox[{MakeBoxes[asset, StandardForm], "/", MakeBoxes[denomination,
         StandardForm]}];


BitstampAccount /: ToString[BitstampAccount[name_]] := ToString[name];

BitstampAccount /: MakeBoxes[BitstampAccount[name_], StandardForm] := RowBox[{MakeBoxes[name, StandardForm]}];


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
    BitstampOrder[data][{props}];


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
