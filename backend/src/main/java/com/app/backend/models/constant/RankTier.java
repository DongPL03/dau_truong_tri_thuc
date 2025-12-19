package com.app.backend.models.constant;

public enum RankTier {
    BRONZE(0, 1.0),
    SILVER(2000, 1.05),
    GOLD(5000, 1.10),
    PLATINUM(10000, 1.15),
    DIAMOND(20000, 1.20),
    MASTER(35000, 1.25);

    private final int minPoints;
    private final double multiplier;

    RankTier(int minPoints, double multiplier) {
        this.minPoints = minPoints;
        this.multiplier = multiplier;
    }

    public double getMultiplier() {
        return multiplier;
    }

    // Hàm tiện ích để tìm Rank dựa trên điểm
    public static RankTier fromPoints(int points) {
        // Duyệt ngược từ cao xuống thấp để tìm rank phù hợp
        RankTier[] tiers = values();
        for (int i = tiers.length - 1; i >= 0; i--) {
            if (points >= tiers[i].minPoints) {
                return tiers[i];
            }
        }
        return BRONZE;
    }
}