package com.app.backend.services.bocauhoi;

import com.app.backend.dtos.BoCauHoiDTO;
import com.app.backend.dtos.TuChoiBoCauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import com.app.backend.models.CauHoi;
import com.app.backend.responses.bocauhoi.UnlockBoCauHoiResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Map;

public interface IBoCauHoiService {
    BoCauHoi create(BoCauHoiDTO boCauHoiDTO, Long currentUserId) throws DataNotFoundException, PermissionDenyException;

    BoCauHoi update(Long id, BoCauHoiDTO boCauHoiDTO, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    void delete(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    BoCauHoi getById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    Page<BoCauHoi> findAllBoCauHoi(PageRequest pageRequest,
                                   String keyword,
                                   Long chuDeId,
                                   String cheDoHienThi,
                                   String trangThai,
                                   String loaiSuDung,
                                   Boolean muonTaoTraPhi,
                                   Long nguoiTaoId,
                                   Long creatorId,
                                   boolean isAdmin);

    BoCauHoi approve(Long id, Long adminId);

    BoCauHoi reject(Long id, TuChoiBoCauHoiDTO lyDoTuChoi, Long adminId);

    void softDelete(Long id, Long userId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    Map<String, Object> thongKeBoCauHoi(Long id) throws DataNotFoundException;

    BoCauHoi markOfficial(Long id, Long adminId) throws DataNotFoundException, PermissionDenyException;

    BoCauHoi disMarkOfficial(Long id, Long adminId) throws DataNotFoundException, PermissionDenyException;

    Page<BoCauHoi> findPracticeSets(PageRequest pageRequest,
                                    Long creatorId,
                                    boolean isAdmin);

    Page<BoCauHoi> findBattleSets(PageRequest pageRequest);

    List<BoCauHoi> getBattleSetsCasualForCurrentUser() throws Exception;

    List<BoCauHoi> getBattleSetsRankedForCurrentUser() throws Exception;

    UnlockBoCauHoiResponse unlockBoCauHoi(Long boCauHoiId, Long userId) throws Exception;

    boolean hasUserUnlockedBo(Long boCauHoiId, Long userId);
}
