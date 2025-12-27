package com.app.backend.services.thongke;

import com.app.backend.responses.thongke.AdminSummaryStatsResponse;
import com.app.backend.responses.thongke.DateCountResponse;
import com.app.backend.responses.thongke.TopBoCauHoiStatsResponse;
import com.app.backend.responses.thongke.TopPlayerStatsResponse;

import java.util.List;

public interface IThongKeAdminService {

    AdminSummaryStatsResponse getSummary();

    /**
     * Thống kê số trận theo ngày, trong X ngày gần đây
     */
    List<DateCountResponse> getBattlesByDay(int days);

    /**
     * Top bộ câu hỏi được dùng nhiều nhất
     */
    List<TopBoCauHoiStatsResponse> getTopBoCauHoi(int limit);

    /**
     * Top người chơi (theo điểm tích lũy)
     */
    List<TopPlayerStatsResponse> getTopPlayers(int limit);
}
