package com.app.backend.services.cauhoi;

import com.app.backend.dtos.CauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import com.app.backend.models.CauHoi;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.ICauHoiRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@RequiredArgsConstructor
public class CauHoiService implements ICauHoiService {
    private final ICauHoiRepository cauHoiRepository;
    private final IBoCauHoiRepository boCauHoiRepository;


    @Override
    public Page<CauHoi> findAll(Long boCauHoiId,
                                String keyword,
                                String doKho,
                                String loaiNoiDung,
                                Long creatorId,
                                boolean isAdmin,
                                PageRequest pageRequest) {
        return cauHoiRepository.searchCauHoi(
                boCauHoiId,
                keyword,
                doKho,
                loaiNoiDung,
                creatorId,
                isAdmin,
                pageRequest
        );
    }

    @Override
    public void changeMedia(Long cauHoiId, String duongDanTep) throws DataNotFoundException {
        CauHoi cauHoi = cauHoiRepository.findById(cauHoiId)
                .orElseThrow(() -> new DataNotFoundException("Câu hỏi không tồn tại"));
        cauHoi.setDuongDanTep(duongDanTep);
        cauHoiRepository.save(cauHoi);
    }

    @Override
    public CauHoi create(CauHoiDTO cauHoiDTO, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(cauHoiDTO.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));
        if (!isAdmin && !boCauHoi.getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được thêm câu hỏi vào bộ do bạn tạo");
        }
        CauHoi cauHoi = CauHoi.builder()
                .boCauHoi(boCauHoi)
                .doKho(cauHoiDTO.getDoKho())
                .noiDung(cauHoiDTO.getNoiDung())
                .loaiNoiDung(cauHoiDTO.getLoaiNoiDung())
                .luaChonA(cauHoiDTO.getLuaChonA())
                .luaChonB(cauHoiDTO.getLuaChonB())
                .luaChonC(cauHoiDTO.getLuaChonC())
                .luaChonD(cauHoiDTO.getLuaChonD())
                .dapAnDung(cauHoiDTO.getDapAnDung())
                .giaiThich(cauHoiDTO.getGiaiThich())
                .build();
        return cauHoiRepository.save(cauHoi);
    }

    @Override
    public CauHoi update(Long id, CauHoiDTO cauHoiDTO, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        CauHoi cauHoi = cauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Câu hỏi không tồn tại"));
        BoCauHoi boCauHoi = cauHoi.getBoCauHoi();
        if (!isAdmin && !boCauHoi.getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được sửa câu hỏi trong bộ của bạn");
        }
        if (cauHoiDTO.getBoCauHoiId() != null && !cauHoiDTO.getBoCauHoiId().equals(boCauHoi.getId())) {
            // Cho phép chuyển câu hỏi sang bộ khác nếu vẫn thuộc sở hữu:
            BoCauHoi newBoCauHoi = boCauHoiRepository.findById(cauHoiDTO.getBoCauHoiId())
                    .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi mới không tồn tại"));
            if (!isAdmin && !newBoCauHoi.getTaoBoi().getId().equals(currentUserId)) {
                throw new PermissionDenyException("Không thể chuyển câu hỏi sang bộ không thuộc sở hữu");
            }
            cauHoi.setBoCauHoi(newBoCauHoi);
        }
        if (cauHoiDTO.getDoKho() != null) cauHoi.setDoKho(cauHoiDTO.getDoKho());
        if (cauHoiDTO.getNoiDung() != null) cauHoi.setNoiDung(cauHoiDTO.getNoiDung());
        if (cauHoiDTO.getLoaiNoiDung() != null) cauHoi.setLoaiNoiDung(cauHoiDTO.getLoaiNoiDung());
        if (cauHoiDTO.getDuongDanTep() != null) cauHoi.setDuongDanTep(cauHoiDTO.getDuongDanTep());
        if (cauHoiDTO.getLuaChonA() != null) cauHoi.setLuaChonA(cauHoiDTO.getLuaChonA());
        if (cauHoiDTO.getLuaChonB() != null) cauHoi.setLuaChonB(cauHoiDTO.getLuaChonB());
        if (cauHoiDTO.getLuaChonC() != null) cauHoi.setLuaChonC(cauHoiDTO.getLuaChonC());
        if (cauHoiDTO.getLuaChonD() != null) cauHoi.setLuaChonD(cauHoiDTO.getLuaChonD());
        if (cauHoiDTO.getDapAnDung() != null) {
            cauHoi.setDapAnDung(Character.toUpperCase(cauHoiDTO.getDapAnDung()));
        }
        if (cauHoiDTO.getGiaiThich() != null) cauHoi.setGiaiThich(cauHoiDTO.getGiaiThich());
        return cauHoiRepository.save(cauHoi);
    }

    @Override
    public void delete(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        CauHoi cauHoi = cauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Câu hỏi không tồn tại"));

        if (!isAdmin && !cauHoi.getBoCauHoi().getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được xoá câu hỏi trong bộ của bạn");
        }
        cauHoiRepository.delete(cauHoi);
    }

    public CauHoi findById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        CauHoi cauHoi = cauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Câu hỏi không tồn tại"));
        if (!isAdmin && !Objects.equals(cauHoi.getBoCauHoi().getCheDoHienThi(), "PUBLIC")
                && !cauHoi.getBoCauHoi().getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn không có quyền xem câu hỏi này");
        }
        return cauHoi;
    }
}
