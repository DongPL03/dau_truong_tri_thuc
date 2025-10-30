package com.app.backend.services.phienluyentap;

import com.app.backend.dtos.BatDauLuyenTapRequestDTO;
import com.app.backend.dtos.TraLoiCauHoiRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TheGhiNho;
import com.app.backend.models.TraLoiLuyenTap;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface ILuyenTapService {
    PhienLuyenTap batDau(BatDauLuyenTapRequestDTO request, Long userId) throws DataNotFoundException;

    PhienLuyenTap guiDapAn(TraLoiCauHoiRequestDTO request, Long userId) throws DataNotFoundException, PermissionDenyException;

    List<TraLoiLuyenTap> getTraLoiByPhien(Long phienId, Long userId) throws DataNotFoundException, PermissionDenyException;

    Page<PhienLuyenTap> getPracticeHistory(Long userId, boolean isAdmin, PageRequest pageRequest);

    TheGhiNho saveTheGhiNho(Long phienId, Long cauHoiId, Long userId) throws DataNotFoundException, PermissionDenyException;

    Page<TheGhiNho> getTheGhiNhoList(Long userId, Pageable pageable);

    void deleteTheGhiNho(Long memoId, Long userId) throws DataNotFoundException, PermissionDenyException;

}
