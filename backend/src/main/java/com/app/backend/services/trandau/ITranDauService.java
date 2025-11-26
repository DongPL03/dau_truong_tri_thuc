package com.app.backend.services.trandau;

import com.app.backend.dtos.*;
import com.app.backend.models.TranDau;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.responses.trandau.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

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

}
