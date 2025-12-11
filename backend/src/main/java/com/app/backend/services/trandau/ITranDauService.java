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

public interface ITranDauService {
    TranDau taoPhong(TaoTranDauDTO dto, Long currentUserId) throws Exception;

    TranDau thamGia(ThamGiaTranDauDTO dto, Long currentUserId) throws Exception;

    void roiPhong(RoiTranDauDTO dto, Long currentUserId) throws Exception;

    TranDau chiTietPhong(Long tranDauId) throws Exception;

    Page<TranDau> danhSachPhongCho(PageRequest pageRequest);

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

}
