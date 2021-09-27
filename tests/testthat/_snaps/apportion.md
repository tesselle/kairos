# Chronological Apportioning - matrix

    Code
      app_uni
    Output
      An object of class "CountApportion"
      , , 1200_1250
      
              CWW CBW    LMGRW LTB MMS PBW RRW SCBW TBBW
      Bayless 1.6 111 3.666667 5.5   0 535   0    3   24
      
      , , 1250_1300
      
              CWW CBW    LMGRW LTB      MMS PBW RRW SCBW TBBW
      Bayless 1.6 111 3.666667 5.5 4.333333 535  84    3   24
      
      , , 1300_1350
      
              CWW CBW    LMGRW LTB      MMS PBW RRW SCBW TBBW
      Bayless 0.8 111 3.666667   0 8.666667 535 168    3    0
      
      Slot "p":
      , , 1200_1250
      
              CWW       CBW     LMGRW LTB MMS       PBW RRW      SCBW TBBW
      Bayless 0.4 0.3333333 0.3333333 0.5   0 0.3333333   0 0.3333333  0.5
      
      , , 1250_1300
      
              CWW       CBW     LMGRW LTB       MMS       PBW       RRW      SCBW
      Bayless 0.4 0.3333333 0.3333333 0.5 0.3333333 0.3333333 0.3333333 0.3333333
              TBBW
      Bayless  0.5
      
      , , 1300_1350
      
              CWW       CBW     LMGRW LTB       MMS       PBW       RRW      SCBW
      Bayless 0.2 0.3333333 0.3333333   0 0.6666667 0.3333333 0.6666667 0.3333333
              TBBW
      Bayless    0
      
      
      Slot "method":
      [1] "uniform"
      
      Slot "from":
      [1] 1200
      
      Slot "to":
      [1] 1350
      
      Slot "step":
      [1] 50
      

---

    Code
      get_weights(app_uni)
    Output
      , , 1200_1250
      
              CWW       CBW     LMGRW LTB MMS       PBW RRW      SCBW TBBW
      Bayless 0.4 0.3333333 0.3333333 0.5   0 0.3333333   0 0.3333333  0.5
      
      , , 1250_1300
      
              CWW       CBW     LMGRW LTB       MMS       PBW       RRW      SCBW
      Bayless 0.4 0.3333333 0.3333333 0.5 0.3333333 0.3333333 0.3333333 0.3333333
              TBBW
      Bayless  0.5
      
      , , 1300_1350
      
              CWW       CBW     LMGRW LTB       MMS       PBW       RRW      SCBW
      Bayless 0.2 0.3333333 0.3333333   0 0.6666667 0.3333333 0.6666667 0.3333333
              TBBW
      Bayless    0
      

---

    Code
      app_trunc
    Output
      An object of class "CountApportion"
      , , 1200_1250
      
                   CWW      CBW    LMGRW      LTB MMS     PBW RRW     SCBW     TBBW
      Bayless 2.114461 152.6501 1.605122 7.513086   0 658.808   0 1.313281 30.86557
      
      , , 1250_1300
      
                   CWW      CBW    LMGRW      LTB      MMS      PBW      RRW     SCBW
      Bayless 1.397752 109.7148 3.990371 3.486914 1.896962 530.2219 37.17403 3.264849
                  TBBW
      Bayless 17.13443
      
      , , 1300_1350
      
                    CWW      CBW    LMGRW LTB      MMS      PBW     RRW    SCBW TBBW
      Bayless 0.4877879 70.63505 5.404507   0 11.10304 415.9701 214.826 4.42187    0
      
      Slot "p":
      , , 1200_1250
      
                    CWW       CBW     LMGRW       LTB MMS       PBW RRW      SCBW
      Bayless 0.5286151 0.4584088 0.1459202 0.6830078   0 0.4104723   0 0.1459202
                   TBBW
      Bayless 0.6430327
      
      , , 1250_1300
      
                    CWW       CBW    LMGRW       LTB       MMS       PBW      RRW
      Bayless 0.3494379 0.3294739 0.362761 0.3169922 0.1459202 0.3303563 0.147516
                  SCBW      TBBW
      Bayless 0.362761 0.3569673
      
      , , 1300_1350
      
                   CWW       CBW     LMGRW LTB       MMS       PBW      RRW      SCBW
      Bayless 0.121947 0.2121173 0.4913188   0 0.8540798 0.2591714 0.852484 0.4913188
              TBBW
      Bayless    0
      
      
      Slot "method":
      [1] "truncated"
      
      Slot "from":
      [1] 1200
      
      Slot "to":
      [1] 1350
      
      Slot "step":
      [1] 50
      

---

    Code
      get_weights(app_trunc)
    Output
      , , 1200_1250
      
                    CWW       CBW     LMGRW       LTB MMS       PBW RRW      SCBW
      Bayless 0.5286151 0.4584088 0.1459202 0.6830078   0 0.4104723   0 0.1459202
                   TBBW
      Bayless 0.6430327
      
      , , 1250_1300
      
                    CWW       CBW    LMGRW       LTB       MMS       PBW      RRW
      Bayless 0.3494379 0.3294739 0.362761 0.3169922 0.1459202 0.3303563 0.147516
                  SCBW      TBBW
      Bayless 0.362761 0.3569673
      
      , , 1300_1350
      
                   CWW       CBW     LMGRW LTB       MMS       PBW      RRW      SCBW
      Bayless 0.121947 0.2121173 0.4913188   0 0.8540798 0.2591714 0.852484 0.4913188
              TBBW
      Bayless    0
      

