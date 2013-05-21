require(magic)
# Create YearMonth field to be used as a quick lookup for data ranges
magicSQL("ALTER TABLE Friendships_Directional_Final 
         ADD YearMonth VARCHAR(6)", "cpw_meta")

magicSQL("UPDATE Friendships_Directional_Final SET YearMonth = concat(cast(Year as char), cast(LPAD(Month, 2, '0') as char))", "cpw_meta")