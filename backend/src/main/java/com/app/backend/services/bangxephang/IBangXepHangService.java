package com.app.backend.services.bangxephang;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.responses.bangxephang.LeaderboardEntryResponse;
import com.app.backend.responses.user.UserSummaryResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

public interface IBangXepHangService {
    Page<LeaderboardEntryResponse> getGlobalLeaderboard(
            PageRequest pageRequest,
            String timeRange,
            Long chuDeId,
            Long boCauHoiId
//            Long currentUserId,
//            boolean friendOnly
    );

    UserSummaryResponse getUserSummary(Long userId) throws DataNotFoundException;

    void recalcAllRankings();
}
