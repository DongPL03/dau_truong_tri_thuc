package com.app.backend.services.trandau;

import com.app.backend.components.BattleLoopTask;
import com.app.backend.components.BattleStateManager;
import com.app.backend.components.BattleWsPublisher;
import com.app.backend.dtos.*;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.models.constant.LuatTinhDiem;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.*;
import com.app.backend.responses.trandau.BattleFinishResponse;
import com.app.backend.responses.trandau.BattleStartResponse;
import com.app.backend.responses.trandau.SubmitAnswerResponse;
import com.app.backend.responses.websocket.FinishedEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TranDauService implements ITranDauService {

    private final ITranDauRepository tranDauRepository;
    private final INguoiChoiTranDauRepository nguoiChoiTranDauRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ICauHoiRepository cauHoiRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final BattleStateManager battleStateManager;
    private final BattleLoopTask battleLoopTask;
    private final BattleWsPublisher wsPublisher;

    private String generateRoomCode(int length) {
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        Random random = new Random();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < length; i++) {
            sb.append(chars.charAt(random.nextInt(chars.length())));
        }
        return sb.toString();
    }

    @Transactional
    @Override
    public TranDau taoPhong(TaoTranDauDTO taoTranDauDTO, Long currentUserId) throws Exception {
        NguoiDung host = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        BoCauHoi bo = boCauHoiRepository.findById(taoTranDauDTO.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (Boolean.FALSE.equals(taoTranDauDTO.getCongKhai()) &&
                (taoTranDauDTO.getMaPin() == null || taoTranDauDTO.getMaPin().isBlank())) {
            throw new IllegalArgumentException("Phòng riêng tư bắt buộc có mã PIN");
        }
        if (taoTranDauDTO.getGioiHanNguoiChoi() < 2 || taoTranDauDTO.getGioiHanNguoiChoi() > 4) {
            throw new IllegalArgumentException("Giới hạn người chơi phải từ 2 – 4");
        }
        TranDau tranDau = new TranDau();
        tranDau.setTenPhong(taoTranDauDTO.getTenPhong());
        tranDau.setBoCauHoi(bo);
        tranDau.setChuPhong(host);
        tranDau.setCongKhai(taoTranDauDTO.getCongKhai());
        tranDau.setMaPin(taoTranDauDTO.getCongKhai() ? null : taoTranDauDTO.getMaPin());
        tranDau.setMaPhong(generateRoomCode(6));
        tranDau.setGioiHanNguoiChoi(taoTranDauDTO.getGioiHanNguoiChoi());
        tranDau.setGioiHanThoiGianCauGiay(taoTranDauDTO.getGioiHanThoiGianCauGiay());
        // Luật tính điểm nếu có enum:
        tranDau.setLuatTinhDiem(taoTranDauDTO.getLuatTinhDiem());
        tranDau.setTrangThai(TrangThaiTranDau.PENDING);

        TranDau saved = tranDauRepository.save(tranDau);

        // tự động cho host vào phòng
        NguoiChoiTranDau hostJoin = new NguoiChoiTranDau();
        hostJoin.setTranDau(saved);
        hostJoin.setNguoiDung(host);
        nguoiChoiTranDauRepository.save(hostJoin);

        return saved;
    }

    @Transactional
    @Override
    public TranDau thamGia(ThamGiaTranDauDTO thamGiaTranDauDTO, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(thamGiaTranDauDTO.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!Objects.equals(tranDau.getTrangThai(), TrangThaiTranDau.PENDING)) {
            throw new IllegalStateException("Phòng không ở trạng thái chờ");
        }

        long current = nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        if (current >= tranDau.getGioiHanNguoiChoi()) {
            throw new IllegalStateException("Phòng đã đủ người");
        }

        // 🔒 Nếu phòng private → yêu cầu mã PIN
        if (Boolean.FALSE.equals(tranDau.getCongKhai())) {
            if (thamGiaTranDauDTO.getMaPin() == null || !thamGiaTranDauDTO.getMaPin().equals(tranDau.getMaPin())) {
                throw new SecurityException("Sai mã PIN hoặc phòng này riêng tư");
            }
        }

        // đã tham gia chưa?
        boolean existed = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDau.getId(), currentUserId)
                .isPresent();
        if (existed) return tranDau; // idempotent

        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        NguoiChoiTranDau join = new NguoiChoiTranDau();
        join.setTranDau(tranDau);
        join.setNguoiDung(user);
        nguoiChoiTranDauRepository.save(join);

        return tranDau;
    }

    @Transactional
    @Override
    public void roiPhong(RoiTranDauDTO dto, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        NguoiChoiTranDau nctd = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDau.getId(), currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa ở trong phòng"));
        // Nếu host rời phòng khi đang PENDING → có thể chuyển host cho người khác hoặc xoá phòng.
        // Bước 1: đơn giản là xoá người chơi ra khỏi phòng.
        nguoiChoiTranDauRepository.delete(nctd);

        // Nếu không còn ai trong phòng → xoá phòng
        long remain = nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());

        if (Objects.equals(tranDau.getChuPhong().getId(), currentUserId) && remain > 0) {
            NguoiChoiTranDau nextHost = nguoiChoiTranDauRepository
                    .findFirstByTranDau_IdOrderByIdAsc(tranDau.getId())
                    .orElse(null);
            if (nextHost != null) tranDau.setChuPhong(nextHost.getNguoiDung());
        }

        if (remain == 0 && Objects.equals(tranDau.getTrangThai(), TrangThaiTranDau.PENDING)) {
            tranDauRepository.delete(tranDau);
        }
    }

    @Transactional(readOnly = true)
    @Override
    public TranDau chiTietPhong(Long tranDauId, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
        return tranDau;
    }

    @Transactional(readOnly = true)
    @Override
    public Page<TranDau> danhSachPhongCho(PageRequest pageRequest) {
        return tranDauRepository.findByTrangThai(TrangThaiTranDau.PENDING, pageRequest);
    }

    @Transactional
    @Override
    public BattleStartResponse startBattle(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // Kiểm tra quyền host
        if (!td.getChuPhong().getId().equals(currentUserId)) {
            throw new SecurityException("Chỉ chủ phòng mới có quyền bắt đầu trận");
        }

        // Kiểm tra trạng thái
        if (!TrangThaiTranDau.PENDING.equals(td.getTrangThai())) {
            throw new IllegalStateException("Phòng không ở trạng thái chờ");
        }

        // Lấy câu hỏi từ bộ câu hỏi
        List<CauHoi> danhSachCauHoi = cauHoiRepository.findByBoCauHoiId(td.getBoCauHoi().getId());
        if (danhSachCauHoi.isEmpty()) {
            throw new IllegalStateException("Bộ câu hỏi này không có câu hỏi nào");
        }

        // Trộn ngẫu nhiên danh sách câu hỏi
        Collections.shuffle(danhSachCauHoi);

        // Cập nhật trạng thái trận
        td.setTrangThai(TrangThaiTranDau.ONGOING);
        td.setBatDauLuc(LocalDateTime.now());
        tranDauRepository.save(td);

        // Khởi tạo trạng thái tạm thời trong memory
        BattleState state = new BattleState();
        state.setTranDauId(td.getId());
        state.setDanhSachCauHoi(danhSachCauHoi);
//        state.setDanhSachCauHoi(
//                danhSachCauHoi.stream()
//                        .map(q -> new CauHoi(q.getId(), q.getNoiDung(), q.getLuaChonA(), q.getLuaChonB(), q.getLuaChonC(), q.getLuaChonD(), q.getDapAnDung()))
//                        .toList()
//        );

        state.setStartTime(LocalDateTime.now());
        battleStateManager.save(state);

        // Bắt đầu vòng lặp auto
        int seconds = td.getGioiHanThoiGianCauGiay() != null ? td.getGioiHanThoiGianCauGiay() : 15;
        battleLoopTask.runAutoLoop(td.getId(), seconds);


        // Trả response
        return BattleStartResponse.from(td, danhSachCauHoi);
    }

//    @Transactional
//    @Override
//    public QuestionPlayResponse nextQuestion(NextQuestionDTO dto, Long currentUserId) throws Exception {
//        TranDau td = tranDauRepository.findById(dto.getTranDauId())
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//
//        // chỉ host được quyền next
//        if (!Objects.equals(td.getChuPhong().getId(), currentUserId)) {
//            throw new SecurityException("Chỉ chủ phòng được quyền bật câu tiếp theo");
//        }
//
//        if (!Objects.equals(td.getTrangThai(), TrangThaiTranDau.ONGOING)) {
//            throw new IllegalStateException("Phòng không ở trạng thái đang diễn ra");
//        }
//
//        BattleState state = battleStateManager.get(td.getId());
//        if (state == null) {
//            throw new IllegalStateException("Trận đấu chưa sẵn sàng hoặc đã kết thúc");
//        }
//
//        // tăng index câu
//        int nextIndex = state.getCurrentQuestionIndex() + 1;
//        if (nextIndex >= state.getDanhSachCauHoi().size()) {
//            throw new IllegalStateException("Đã hết câu hỏi");
//        }
//
//        state.setCurrentQuestionIndex(nextIndex);
//        state.setCurrentQuestionStart(LocalDateTime.now());
//        state.getAnswers().putIfAbsent(nextIndex, new ConcurrentHashMap<>());
//        battleStateManager.save(state);
//
//        CauHoi q = state.getDanhSachCauHoi().get(nextIndex);
//        int seconds = td.getGioiHanThoiGianCauGiay() != null ? td.getGioiHanThoiGianCauGiay() : 15;
//
//        return QuestionPlayResponse.from(q, seconds);
//    }

    // ----------- BỔ SUNG 2: SUBMIT ANSWER -----------
    @Transactional
    @Override
    public SubmitAnswerResponse submitAnswer(SubmitAnswerDTO dto, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!Objects.equals(td.getTrangThai(), TrangThaiTranDau.ONGOING)) {
            throw new IllegalStateException("Phòng không ở trạng thái đang diễn ra");
        }

        // kiểm tra user đang trong phòng
        boolean inRoom = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(td.getId(), currentUserId)
                .isPresent();
        if (!inRoom && !Objects.equals(td.getChuPhong().getId(), currentUserId)) {
            throw new SecurityException("Bạn không ở trong phòng này");
        }

        BattleState state = battleStateManager.get(td.getId());
        if (state == null || state.getCurrentQuestionIndex() < 0) {
            throw new IllegalStateException("Chưa có câu hỏi nào đang bật");
        }

        int idx = state.getCurrentQuestionIndex();
        CauHoi currentQ = state.getDanhSachCauHoi().get(idx);
        if (!Objects.equals(currentQ.getId(), dto.getCauHoiId())) {
            throw new IllegalArgumentException("Câu hỏi không khớp với câu hiện tại");
        }

        // chống nộp nhiều lần
        Map<Long, String> answered = state.getAnswers().getOrDefault(idx, new HashMap<>());
        if (answered.containsKey(currentUserId)) {
            throw new IllegalStateException("Bạn đã nộp đáp án cho câu này");
        }

        // kiểm tra timeout
        int seconds = td.getGioiHanThoiGianCauGiay() != null ? td.getGioiHanThoiGianCauGiay() : 15;
        long totalMillis = seconds * 1000L;
        long elapsedMillis = Duration.between(state.getCurrentQuestionStart(), LocalDateTime.now()).toMillis();
        if (elapsedMillis > totalMillis) {
            // hết giờ → coi như sai, 0 điểm (có thể cho phép late submit = 0 điểm)
            answered.put(currentUserId, dto.getAnswer().toUpperCase());
            state.getAnswers().put(idx, answered);
            battleStateManager.save(state);

            int totalPoints = state.getDiemNguoiChoi().getOrDefault(currentUserId, 0);
            return SubmitAnswerResponse.builder()
                    .correct(false)
                    .gainedPoints(0)
                    .totalPoints(totalPoints)
                    .questionIndex(idx)
                    .build();
        }

        // chấm điểm
        String ans = dto.getAnswer().trim().toUpperCase();
        boolean correct = ans.equalsIgnoreCase(String.valueOf(currentQ.getDapAnDung()));

        int gained = 0;
        if (correct) {
            if (LuatTinhDiem.SPEED_BONUS.equalsIgnoreCase(td.getLuatTinhDiem())) {
                // linear speed bonus: 100..1000 điểm theo thời gian còn lại
                long remain = Math.max(0, totalMillis - elapsedMillis);
                double ratio = (double) remain / (double) totalMillis; // 0..1
                gained = (int) Math.max(100, Math.round(1000 * ratio));
            } else { // BASIC
                gained = 100; // cố định
            }
        }

        // cập nhật điểm
        int total = state.getDiemNguoiChoi().getOrDefault(currentUserId, 0) + gained;
        state.getDiemNguoiChoi().put(currentUserId, total);

        // lưu answer
        answered.put(currentUserId, ans);
        state.getAnswers().put(idx, answered);

        battleStateManager.save(state);

        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));
        wsPublisher.publishScoreUpdate(td.getId(), currentUserId, user.getHoTen(), correct, gained, total, idx);

        return SubmitAnswerResponse.builder()
                .correct(correct)
                .gainedPoints(gained)
                .totalPoints(total)
                .questionIndex(idx)
                .build();
    }

    @Transactional
    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
        }

        // Nếu đã kết thúc rồi, trả kết quả cũ
        if ("FINISHED".equals(td.getTrangThai())) {
            return BattleFinishResponse.from(td, null, null);
        }

        // ✅ Lấy BattleState trong RAM
        BattleState state = battleStateManager.get(td.getId());
        Map<Long, Integer> scores = (state != null) ? state.getDiemNguoiChoi() : new HashMap<>();

        // ✅ Tính người thắng
        Long winnerId = null;
        String winnerTen = null;
        if (!scores.isEmpty()) {
            // Lấy người có điểm cao nhất
            var topEntry = scores.entrySet().stream()
                    .max(Map.Entry.comparingByValue())
                    .orElse(null);
            if (topEntry != null) {
                winnerId = topEntry.getKey();
                NguoiDung winnerUser = nguoiDungRepository.findById(winnerId).orElse(null);
                winnerTen = (winnerUser != null) ? winnerUser.getHoTen() : "Người chơi";
                td.setWinner(winnerUser);
            }
        }

        // ✅ Cập nhật DB
        td.setTrangThai("FINISHED");
        td.setKetThucLuc(LocalDateTime.now());
        tranDauRepository.save(td);

        // ✅ Lấy danh sách người chơi (để hiển thị tên)
        List<NguoiDung> allUsers = nguoiDungRepository.findAllById(scores.keySet());
        Map<Long, String> nameMap = allUsers.stream()
                .collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen));

        // ✅ Tạo danh sách bảng điểm (sắp giảm dần theo điểm)
        AtomicInteger rankCounter = new AtomicInteger(1);
        List<BattleFinishResponse.PlayerScore> list = scores.entrySet().stream()
                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
                .map(e -> BattleFinishResponse.PlayerScore.builder()
                        .userId(e.getKey())
                        .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
                        .diem(e.getValue())
                        .thuHang(rankCounter.getAndIncrement())
                        .build())
                .toList();


        // ✅ Dọn cache state
        battleStateManager.remove(td.getId());

        // ✅ --- PHÁT SỰ KIỆN WEBSOCKET ---
        FinishedEvent.Winner win = (winnerId != null)
                ? FinishedEvent.Winner.builder().userId(winnerId).hoTen(winnerTen).build()
                : null;

        List<FinishedEvent.Player> players = list.stream()
                .map(p -> FinishedEvent.Player.builder()
                        .userId(p.getUserId())
                        .hoTen(p.getHoTen())
                        .diem(p.getDiem())
                        .thuHang(p.getThuHang())
                        .build())
                .toList();

        wsPublisher.publishFinished(
                td.getId(),
                td.getTenPhong(),
                td.getMaPhong(),
                td.getBatDauLuc(),
                td.getKetThucLuc(),
                win,
                players
        );

        // ✅ Trả response cuối cùng cho API
        return BattleFinishResponse.from(td, scores, allUsers);
    }
}
