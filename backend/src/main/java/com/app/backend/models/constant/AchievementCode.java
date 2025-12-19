package com.app.backend.models.constant;

import lombok.Getter;

@Getter
public enum AchievementCode {

    // 🏟 Trận đấu & chiến thắng
    FIRST_MATCH("FIRST_MATCH", "Trận đầu tiên", "Hoàn thành 1 trận đấu bất kỳ"),
    TEN_MATCHES("TEN_MATCHES", "Lính mới chăm chỉ", "Hoàn thành 10 trận đấu"),
    FIFTY_MATCHES("FIFTY_MATCHES", "Cao thủ cày cuốc", "Hoàn thành 50 trận đấu"),
    HUNDRED_MATCHES("HUNDRED_MATCHES", "Huyền thoại đấu trường", "Hoàn thành 100 trận đấu"),

    FIRST_WIN("FIRST_WIN", "Chiến thắng đầu tiên", "Thắng 1 trận đấu bất kỳ"),
    TEN_WINS("TEN_WINS", "Chuỗi chiến thắng", "Thắng tổng cộng 10 trận"),
    FIFTY_WINS("FIFTY_WINS", "Cao thủ lão luyện", "Thắng tổng cộng 50 trận"),


    // 🎚 Level
    LEVEL_5("LEVEL_5", "Tân binh lên hạng", "Đạt cấp độ 5"),
    LEVEL_10("LEVEL_10", "Chiến binh dày dạn", "Đạt cấp độ 10"),
    LEVEL_20("LEVEL_20", "Bậc thầy tri thức", "Đạt cấp độ 20"),
    LEVEL_30("LEVEL_30", "Huyền thoại đấu trường", "Đạt cấp độ 30"),
    LEVEL_40("LEVEL_40", "Thần đồng chiến thắng", "Đạt cấp độ 40"),
    LEVEL_50("LEVEL_50", "Vô địch Đấu Trường", "Đạt cấp độ 50"),

    // 💰 Vàng
    GOLD_350("GOLD_350", "Người chơi tiềm năng", "Tích lũy ít nhất 350 vàng"),
    GOLD_400("GOLD_400", "Chiến binh dũng mãnh", "Tích lũy ít nhất 400 vàng"),
    GOLD_500("GOLD_500", "Tay chơi có điều kiện", "Tích lũy ít nhất 500 vàng"),
    GOLD_1000("GOLD_1000", "Chiến binh giàu có", "Tích lũy ít nhất 1000 vàng"),
    GOLD_2000("GOLD_2000", "Đại gia Đấu Trường", "Tích lũy ít nhất 2000 vàng"),

    // 🏅 Rank tier
    REACH_SILVER("REACH_SILVER", "Bước vào Bạc", "Đạt rank SILVER hoặc cao hơn"),
    REACH_GOLD("REACH_GOLD", "Vươn tới Vàng", "Đạt rank GOLD hoặc cao hơn"),
    REACH_PLATINUM("REACH_PLATINUM", "Chạm tới Bạch Kim", "Đạt rank PLATINUM hoặc cao hơn"),
    REACH_DIAMOND("REACH_DIAMOND", "Chiến binh Kim Cương", "Đạt rank DIAMOND hoặc cao hơn"),
    REACH_MASTER("REACH_MASTER", "Bậc thầy Đấu Trường", "Đạt rank MASTER");

    private final String code;
    private final String title;
    private final String description;

    AchievementCode(String code, String title, String description) {
        this.code = code;
        this.title = title;
        this.description = description;
    }
}

