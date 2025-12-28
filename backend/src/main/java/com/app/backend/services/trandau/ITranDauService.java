package com.app.backend.services.trandau;

import com.app.backend.dtos.*;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.TranDau;
import com.app.backend.responses.admin.QuestionAnswersAdminResponse;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.responses.trandau.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Map;

public interface ITranDauService {
    TranDau taoPhong(TaoTranDauDTO dto, Long currentUserId) throws Exception;

    TranDau thamGia(ThamGiaTranDauDTO dto, Long currentUserId) throws Exception;

    void roiPhong(RoiTranDauDTO dto, Long currentUserId) throws Exception;

    TranDau chiTietPhong(Long tranDauId) throws Exception;

    Page<TranDau> danhSachPhongCho(PageRequest pageRequest);
    Page<TranDau> danhSachPhongCho(PageRequest pageRequest, String loaiTranDau);


    BattleStartResponse startBattle(Long tranDauId, Long currentUserId) throws Exception;

    SubmitAnswerResponse submitAnswer(SubmitAnswerDTO dto, Long currentUserId) throws Exception;

    BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception;

    SyncStateResponse syncState(Long tranDauId, Long currentUserId) throws Exception;

    Page<LichSuTranDauResponse> getMyHistory(Long currentUserId, int page, int limit);

    LichSuTranDauDetailResponse getMyHistoryDetail(Long tranDauId, Long currentUserId) throws Exception;

    void guiChatTrongTran(GuiChatDTO dto, Long currentUserId) throws Exception;

    TranDauResponse getBattleDetailResponse(Long tranDauId) throws Exception;

    TranDauResponse getBattleDetailResponse(Long tranDauId, Long currentUserId) throws Exception;

    Page<LichSuTranDauResponse> getUserHistory(Long userId, int page, int limit);

    Page<LichSuTranDauResponse> getAllHistory(int page, int limit);

    LichSuTranDauDetailResponse getHistoryDetailAdmin(Long lichSuId) throws Exception;

    // 1) Admin xem chi tiết từng câu của 1 user trong trận
    List<LichSuTranDauQuestionResponse> getPlayerAnswersAdmin(Long tranDauId, Long userId) throws DataNotFoundException;

    // 2) Admin xem tất cả người chơi của 1 câu hỏi
    QuestionAnswersAdminResponse getQuestionAnswersAdmin(Long tranDauId, Long cauHoiId) throws Exception;

    void inviteFriendToBattle(Long tranDauId, Long currentUserId, Long targetUserId)
            throws Exception;

    // ===================== ADMIN METHODS =====================

    /**
     * Lấy thống kê trận đấu cho admin dashboard
     */
    Map<String, Object> getAdminBattleStats();

    /**
     * Lấy lịch sử trận đấu với filter nâng cao
     */
    Page<LichSuTranDauResponse> getAdminHistoryFiltered(
            int page, int limit, String keyword, String loaiTranDau,
            Long boCauHoiId, String fromDate, String toDate
    );

    /**
     * Admin đóng/hủy phòng đang chờ
     */
    void adminCloseRoom(Long tranDauId) throws Exception;

    /**
     * Admin kick người chơi khỏi phòng
     */
    void adminKickPlayer(Long tranDauId, Long userId) throws Exception;

    /**
     * Admin xóa lịch sử trận đấu
     */
    void adminDeleteHistory(Long lichSuId) throws Exception;

    /**
     * Admin lấy chi tiết phòng đang chờ
     */
    Map<String, Object> adminGetRoomDetail(Long tranDauId) throws Exception;

    /**
     * Export lịch sử trận đấu ra CSV
     */
    byte[] exportHistoryCsv(String keyword, String loaiTranDau, Long boCauHoiId, String fromDate, String toDate);

    /**
     * Lấy danh sách người chơi trong phòng (trước khi trận đấu bắt đầu)
     */
    List<NguoiChoiTrongPhongResponse> getPlayersInRoom(Long tranDauId) throws Exception;

}
